try:
    from setuptools import setup
except ImportError:
    from distutils.cor import setup

config = {
    'description': 'Sample Project',
    'author': 'Tobin Harding',
    'url': 'Where to download it.',
    'author_email': 'me@tobin.cc',
    'version': '0.1',
    'install_requires': ['nose'],
    'packages': ['NAME'],
    'scripts': [],
    'name': 'sample'
}

setup(**config)
