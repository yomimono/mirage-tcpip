# default tests

wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-opam.sh
bash -ex .travis-opam.sh

# build and upload coverage info

eval `opam config env`
dune clean #make sure we rebuild everything with bisect set
BISECT_ENABLE=yes dune runtest
bisect-ppx-report -I _build/default \
        -coveralls coverage.json -service-name travis-ci \
        -service-job-id $TRAVIS_JOB_ID \
        `find . -name 'bisect*.out'`
curl -L -f json_file=@./coverage.json https://coveralls.io/api/v1/jobs

# build the examples

export OPAMYES=1
eval `opam config env`

cd examples/unikernel
git log --oneline |head -5

opam install mirage
mirage configure -t $MIRAGE_MODE $FLAGS
make depend
make 

