GHDL=ghdl
GHDLOPTS=


NAME=main

MAIN=$(NAME).vhdl
RUNTIME=runtime.vhdl



NS=200

vhdl.run: $(RUNTIME) $(MAIN) tb_$(NAME).vhdl
	$(GHDL) -a $(GHDLOPTS) $(RUNTIME)
	$(GHDL) -a $(GHDLOPTS) $(MAIN)
	$(GHDL) -a $(GHDLOPTS) tb_$(NAME).vhdl
	$(GHDL) -e $(GHDLOPTS) tb_$(NAME)
	$(GHDL) -r $(GHDLOPTS) tb_$(NAME) --vcd=tb.vcd --stop-time=$(NS)ns

clean:
	rm -f *~
	rm -f *.o *.dot *.vhd *.cf
	rm -f tb tb.vcd

clean-all:	clean
	rm -f main.vhdl tb_main.vhdl
