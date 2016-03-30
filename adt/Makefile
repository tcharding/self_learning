BUILD_DIRS = lists vectors

# Dummy targets for building and clobbering everything in all subdirectories

all: 	
	@ for dir in ${BUILD_DIRS}; do (cd $${dir}; ${MAKE}) ; done

clean: 
	@ for dir in ${BUILD_DIRS}; do (cd $${dir}; ${MAKE} clean) ; done
