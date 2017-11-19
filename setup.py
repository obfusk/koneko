from setuptools import setup, find_packages
import koneko

setup(
  name              = "koneko",
  url               = "https://github.com/obfusk/koneko",
  description       = "...",  # TODO
  version           = koneko.__version__,
  author            = "Felix C. Stegerman",
  author_email      = "flx@obfusk.net",
  license           = "GPLv3+",
  classifiers       = [
    "Development Status :: 4 - Beta",
    "Environment :: Console",
    "Intended Audience :: Developers",
    "License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)",
    "License :: OSI Approved :: GNU Lesser General Public License v3 or later (LGPLv3+)",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.2",
    "Programming Language :: Python :: 3.3",
    "Programming Language :: Python :: 3.4",
    "Programming Language :: Python :: 3.5",
    "Programming Language :: Python :: 3.6",
    "Programming Language :: Python :: Implementation :: CPython",
    "Programming Language :: Python :: Implementation :: PyPy",
    "Topic :: Software Development :: Interpreters",
    "Topic :: Software Development :: Libraries",
  ],
  keywords          = "...",  # TODO
  packages          = find_packages(),
  entry_points      = { "console_scripts": ["koneko=koneko:main_"] },
  python_requires   = ">=3.2",
  install_requires  = ["pyparsing"],
  extras_require    = { "test": ["coverage"] },
  package_data      = { "koneko": ["lib/*.knk"] },
)
