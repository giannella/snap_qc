"""Entry point for the national per-rule error profiles (issues #2 and #1 follow-up).

    python runners/run_rule_error_profiles.py

Reads methods/anyerror_blended_holdout_2024/, reg_model_data.csv and
qc_data/qc_pub_fy{2022,2023,2024}.sav; writes methods/rule_error_profiles/.
Mines nothing and changes no delivered list.
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, "methods"))

from rule_error_profiles import main  # noqa: E402

if __name__ == "__main__":
    if len(sys.argv) == 1:
        sys.argv += ["--stage", "3"]
    main()
