# Load env variables from .env file
ifneq (,$(wildcard ./.env))
    include .env
    export
endif

.PHONY: sprt, profile, help

help:
	@echo 'Run `make sprt` to run sprt match. Must have ENGINE1_DIR, ENGINE2_DIR, FAST_CHESS_EXE env variables defined in .env file'
	@echo 'Run `make profile` to run engine in profiler mode, reading UCI input from `profile_input.txt`'

sprt:
	rm -f ENGINE1
	rm -f ENGINE2
	(PWD=$$(pwd) ; cd ${ENGINE1_DIR} ; cabal build ; echo "Built engine at $$(cabal exec which chessengine_uci)" ; cp $$(cabal exec which chessengine_uci) ${PWD}/ENGINE1)
	(PWD=$$(pwd) ; cd ${ENGINE2_DIR} ; cabal build ; echo "Built engine at $$(cabal exec which chessengine_uci)" ; cp $$(cabal exec which chessengine_uci) ${PWD}/ENGINE2)
	${FAST_CHESS_EXE} -log file=sprt.log level=trace -pgnout file=games.pgn -engine cmd=ENGINE1 name=ENGINE1 -engine cmd=ENGINE2 name=ENGINE2 -each tc=10+1 -rounds 20 -repeat -concurrency 2 -randomseed -openings file=8moves_v3.pgn format=pgn -sprt elo0=0 elo1=10 alpha=0.05 beta=0.05

profile:
	cabal build --enable-profiling --enable-library-profiling
	cat profile_input.txt | $$(cabal exec which chessengine_uci) +RTS -p

deploy:
	cabal build
	rm ${DEPLOYMENT_DIR}/chessengine_uci
	cp $$(cabal exec which chessengine_uci) ${DEPLOYMENT_DIR}/chessengine_uci