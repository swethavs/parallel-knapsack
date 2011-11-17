X10_PATH=/opt/x10-2.2.0.1/bin

# environment variables
X10_NTHREADS := 24
#X10_STATIC_THREADS := false
#X10LAUNCHER_NPROCS := 1

NUM_ASYNCS=8

knapsack:
	$(X10_PATH)/x10c++ -t -v -report postcompile=1 -o Knapsack.exe -optimize -O -NO_CHECKS Knapsack.x10

functests: knapsack
	salloc -n1 srun.x10sock ./Knapsack.exe 3 12 $(NUM_ASYNCS) true > Knapsack.1.out
	@echo "Dumping contents of Knapsack.1.out ... "
	@grep "" Knapsack.1.out
	@echo " "
	@echo "Find your results in Knapsack.1.out"
	@echo " "

perftests: knapsack
	salloc -n1 srun.x10sock ./Knapsack.exe 10000 10000 $(NUM_ASYNCS) false > Knapsack.3.out
	@echo "Dumping contents of Knapsack.3.out ... "
	@grep "" Knapsack.3.out
	@echo " "
	@echo "Find your results in Knapsack.3.out"
	@echo " "


.PRECIOUS: 
.x10: 
	@echo $@
#	$(X10_PATH)/x10c++ -t -v -report postcompile=1 -o $@ -x10rt mpi -optimize -O -NO_CHECKS  $<


.PHONY: $(P1).out clean
clean:
	rm -f *.cc *.h *.exe *.inc *.out *.mpi *~ \#*
