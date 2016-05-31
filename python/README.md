binst: Python port
=================

This port relies on various packages. See `requirements.txt` for details.

Installation
============

```
python setup.py install
```

Generating Documentation
========================

Make sure Sphinx is installed (`pip install sphinx`).

Then run in command line:

```
sphinx-apidoc -o doc binst --force
cd doc
make html
```
