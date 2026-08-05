"""Entry point for the at-max-benefit diagnostic (issue #1).

    python runners/run_at_max_benefit_diagnostic.py

Reads state_delivery_lists/*.csv and reg_model_data.csv, writes
methods/at_max_benefit_diagnostic/. Read-only against the data; mines nothing.
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, "methods"))

from at_max_benefit_diagnostic import main  # noqa: E402

if __name__ == "__main__":
    main()
