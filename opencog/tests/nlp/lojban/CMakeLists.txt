
LINK_DIRECTORIES(${CMAKE_BINARY_DIR}/opencog/nlp/lojban/CWrapper/)

LINK_LIBRARIES(
	${ATOMSPACE_LIBRARY}
	${COGSERVER_LIBRARIES}
	LojbanModule
)

ADD_CXXTEST(LojbanModuleTest)
