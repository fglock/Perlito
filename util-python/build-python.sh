
rm -rf libpy/

mkdir libpy
touch libpy/__init__.py

mkdir libpy/miniperl6
touch libpy/miniperl6/__init__.py

mkdir libpy/miniperl6/python
touch libpy/miniperl6/python/__init__.py

cp lib/MiniPerl6/Python/Runtime.py libpy/miniperl6/python/runtime.py

