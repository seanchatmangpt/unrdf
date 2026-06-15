test:
    pnpm test

test-full:
    pnpm test

polish:
    make lint

build:
    make build

clean:
    make clean

ci: polish test
