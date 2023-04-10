TARGET=main

$(TARGET): main.ml
	ocamlopt -o $@ $<

run: $(TARGET)
	./$(TARGET)

clean:
	rm $(TARGET) *.cmi *.cmx *.o

all: clean run;
