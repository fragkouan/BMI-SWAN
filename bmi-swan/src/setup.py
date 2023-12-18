from distutils.core import setup, Extension
from distutils.command.build_ext import build_ext
from Cython.Build import cythonize
import numpy
# import os
#
# os.environ["CC"] = "gcc"
# os.environ["CXX"] = "gcc"


bmi_lib = '/home/s2006658/myenv/local/lib'
swan_lib = '/home/s2006658/Dropbox/bitbucket_repo/bmi_swan_atropos/swan'

class custom_build_ext(build_ext):
    def build_extensions(self):
        # Override the compiler executables. Importantly, this
        # removes the "default" compiler flags that would
        # otherwise get passed on to to the compiler, i.e.,
        # distutils.sysconfig.get_var("CFLAGS").
        self.compiler.set_executable("compiler_so", "gcc")
        self.compiler.set_executable("compiler_cxx", "gcc")
        self.compiler.set_executable("compiler_c", "gcc")
        self.compiler.set_executable("linker_so", "gcc")
        build_ext.build_extensions(self)

ext_modules = [
    Extension(
        'bmi_swan',
        ['swanbmi.pyx'],
        libraries=[ 'swanmodel', 'bmif', 'bmiswan'],
        library_dirs = [bmi_lib, swan_lib],
        runtime_library_dirs = [bmi_lib],
        include_dirs = ['./', '/usr/local/include', '/home/s2006658/myenv/local/include',
            numpy.get_include() ],#
        extra_objects = ['bmi_interoperability.o'],
        language = 'c'
        )
]


setup(
    ext_modules=cythonize(ext_modules),
    # cmdclass={"build_ext": custom_build_ext},
)
