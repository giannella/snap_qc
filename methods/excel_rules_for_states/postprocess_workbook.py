"""
Stage 2 — inject the native Excel features openpyxl cannot write.

  1. Native cell checkboxes  (Excel 365 "Insert > Checkbox" control)
  2. Drop the stale calculation chain and force a full recalc on load
     (a leftover calcChain.xml after cell moves is what makes Excel show
      "we found a problem with some content...")

Usage:  python postprocess_workbook.py <workbook.xlsx> [checkbox_cells.json]

The JSON maps sheet name -> list of cell refs that hold TRUE/FALSE and should
render as checkboxes, e.g. {"Summary": ["M2", "M15", ...]}.
"""
import json
import os
import re
import shutil
import sys
import zipfile
import xml.etree.ElementTree as ET

FPB_PART = 'xl/featurePropertyBag/featurePropertyBag.xml'
FPB_XML = (
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
    '<FeaturePropertyBags xmlns="http://schemas.microsoft.com/office/spreadsheetml/2022/'
    'featurepropertybag"><bag type="Checkbox"/><bag type="XFControls">'
    '<bagId k="CellControl">0</bagId></bag><bag type="XFComplement">'
    '<bagId k="XFControls">1</bagId></bag>'
    '<bag type="XFComplements" extRef="XFComplementsMapperExtRef">'
    '<a k="MappedFeaturePropertyBags"><bagId>2</bagId></a></bag></FeaturePropertyBags>')
FPB_REL_TYPE = 'http://schemas.microsoft.com/office/2022/11/relationships/FeaturePropertyBag'
FPB_CONTENT_TYPE = 'application/vnd.ms-excel.featurepropertybag+xml'
XF_EXT = ('<extLst><ext uri="{C7286773-470A-42A8-94C5-96B5CB345126}" '
          'xmlns:xfpb="http://schemas.microsoft.com/office/spreadsheetml/2022/featurepropertybag">'
          '<xfpb:xfComplement i="0"/></ext></extLst>')


def split_xfs(body):
    """Split a <cellXfs> body into its top-level <xf> elements."""
    out, depth, buf = [], 0, ''
    for tok in re.findall(r'<[^>]+>', body):
        buf += tok
        if tok.startswith('<xf'):
            if tok.endswith('/>'):
                if depth == 0:
                    out.append(buf); buf = ''
            else:
                depth += 1
        elif tok == '</xf>':
            depth -= 1
            if depth == 0:
                out.append(buf); buf = ''
    assert buf.strip() == '', 'unparsed trailing content in cellXfs'
    return out


def checkbox_xf(xf):
    """Return a copy of `xf` carrying the cell-control link."""
    if xf.endswith('/>'):
        return xf[:-2] + '>' + XF_EXT + '</xf>'
    return xf[:-len('</xf>')] + XF_EXT + '</xf>'


def sheet_parts(zin):
    """sheet name -> part path (attribute-order independent)."""
    NS_M = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
    NS_R = '{http://schemas.openxmlformats.org/officeDocument/2006/relationships}'
    NS_P = '{http://schemas.openxmlformats.org/package/2006/relationships}'
    targets = {rel.get('Id'): rel.get('Target')
               for rel in ET.fromstring(zin.read('xl/_rels/workbook.xml.rels'))
               if rel.tag == NS_P + 'Relationship'}
    out = {}
    for sh in ET.fromstring(zin.read('xl/workbook.xml')).iter(NS_M + 'sheet'):
        t = targets[sh.get(NS_R + 'id')]
        out[sh.get('name')] = t.lstrip('/') if t.startswith('/') else 'xl/' + t
    return out


def process(path, checkbox_cells):
    zin = zipfile.ZipFile(path)
    parts = {n: zin.read(n) for n in zin.namelist()}
    names = list(parts)
    smap = sheet_parts(zin)
    zin.close()

    # ── 1. checkboxes ────────────────────────────────────────────────────────
    n_boxes = 0
    if checkbox_cells:
        styles = parts['xl/styles.xml'].decode()
        m = re.search(r'<cellXfs count="(\d+)">(.*?)</cellXfs>', styles, re.S)
        xfs = split_xfs(m.group(2))
        assert len(xfs) == int(m.group(1)), 'cellXfs count mismatch'
        clone_of = {}                       # original xf index -> checkbox xf index

        for sheet, refs in checkbox_cells.items():
            part = smap[sheet]
            xml = parts[part].decode()
            for ref in refs:
                cm = re.search(r'<c r="%s"(?: s="(\d+)")?[^>]*?(?:/>|>.*?</c>)' % ref, xml)
                if not cm:
                    raise SystemExit(f'{sheet}!{ref}: cell not found')
                src = int(cm.group(1) or 0)
                if src not in clone_of:
                    xfs.append(checkbox_xf(xfs[src]))
                    clone_of[src] = len(xfs) - 1
                val = re.search(r'<v>([^<]*)</v>', cm.group(0))
                val = (val.group(1) if val else '0').strip()
                val = '1' if val in ('1', 'true', 'TRUE') else '0'
                xml = xml.replace(
                    cm.group(0), f'<c r="{ref}" s="{clone_of[src]}" t="b"><v>{val}</v></c>', 1)
                n_boxes += 1
            parts[part] = xml.encode()

        styles = styles.replace(
            m.group(0), f'<cellXfs count="{len(xfs)}">' + ''.join(xfs) + '</cellXfs>', 1)
        parts['xl/styles.xml'] = styles.encode()

        if FPB_PART not in parts:
            parts[FPB_PART] = FPB_XML.encode()
            names.append(FPB_PART)
            rels = parts['xl/_rels/workbook.xml.rels'].decode()
            rid = 'rId%d' % (max(int(x) for x in re.findall(r'Id="rId(\d+)"', rels)) + 1)
            parts['xl/_rels/workbook.xml.rels'] = rels.replace(
                '</Relationships>',
                f'<Relationship Id="{rid}" Type="{FPB_REL_TYPE}" '
                f'Target="featurePropertyBag/featurePropertyBag.xml"/></Relationships>').encode()
            ct = parts['[Content_Types].xml'].decode()
            parts['[Content_Types].xml'] = ct.replace(
                '</Types>',
                f'<Override PartName="/{FPB_PART}" ContentType="{FPB_CONTENT_TYPE}"/></Types>'
            ).encode()

    # ── 2. calc chain + full recalc ──────────────────────────────────────────
    if 'xl/calcChain.xml' in parts:
        del parts['xl/calcChain.xml']
        names.remove('xl/calcChain.xml')
        parts['xl/_rels/workbook.xml.rels'] = re.sub(
            r'<Relationship [^>]*calcChain[^>]*/>', '',
            parts['xl/_rels/workbook.xml.rels'].decode()).encode()
        parts['[Content_Types].xml'] = re.sub(
            r'<Override PartName="/xl/calcChain\.xml"[^>]*/>', '',
            parts['[Content_Types].xml'].decode()).encode()
    wbx = parts['xl/workbook.xml'].decode()
    cm = re.search(r'<calcPr[^/]*/>', wbx)
    if cm and 'fullCalcOnLoad' not in cm.group(0):
        wbx = wbx.replace(cm.group(0), cm.group(0)[:-2] + ' fullCalcOnLoad="1"/>')
    elif not cm:
        wbx = wbx.replace('</workbook>', '<calcPr calcId="191029" fullCalcOnLoad="1"/></workbook>')
    parts['xl/workbook.xml'] = wbx.encode()

    # ── 3. write back, validating every XML part ─────────────────────────────
    for n, blob in parts.items():
        if n.endswith('.xml') or n.endswith('.rels'):
            ET.fromstring(blob)
    tmp = path + '.tmp'
    with zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as zout:
        for n in names:
            zout.writestr(n, parts[n])
    shutil.move(tmp, path)
    return n_boxes


def main():
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    path = sys.argv[1]
    cfg = sys.argv[2] if len(sys.argv) > 2 else os.path.join(
        os.path.dirname(os.path.abspath(path)) or '.', '.checkbox_cells.json')
    if not os.path.exists(cfg):
        cfg = os.path.join(os.path.dirname(os.path.abspath(__file__)), '.checkbox_cells.json')
    cells = json.load(open(cfg)) if os.path.exists(cfg) else {}
    n = process(path, cells)
    print(f'post-processed {os.path.basename(path)}: {n} checkboxes, calcChain dropped')


if __name__ == '__main__':
    main()
