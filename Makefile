OBJECTS := $(wildcard */*.o | */*.cmx | */*.cmi)
EXE := $(wildcard */*.exe)

clean: $(OBJECTS)
	rm -f $(OBJECTS)

cleanall: clean $(EXE)
	rm -f $(EXE)