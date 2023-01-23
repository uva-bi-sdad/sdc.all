from pathlib import Path
import logging
from string import Template
from pytz import timezone
from datetime import datetime
from abc import ABC, abstractmethod
import inspect


class Test(ABC):
    def __init__(self, file, name, description=None):
        self.name = name
        self.description = description
        self.template = Template(
            """
            <html>
                <head>
                <title>$test_name</title>
                </head>
                <body>
                    Last updated: $time_checked
                    $report
                </body>
            </html>
        """
        )
        self.scriptname = Path(file).resolve().stem

    def export_html(self, report):
        tz = timezone("EST")
        time_checked = datetime.now(tz).strftime("%Y-%m-%d %H:%M:%S")

        logging.info("Time checked: %s" % time_checked)
        print("Exporting to: %s" % self.scriptname)
        with open("./docs/tests/%s.html" % (self.scriptname), "w") as f:
            f.write(
                self.template.substitute(
                    test_name=self.name, time_checked=time_checked, report=report
                )
            )
