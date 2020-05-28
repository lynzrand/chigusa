FROM psychonaut/rust-nightly:2020-05-12 as BASE
WORKDIR /app/chigusa/
COPY Cargo.toml Cargo.lock src crates res stdlib .git /app/chigusa/
RUN git submodules init && git submodules update -r
RUN cargo build --release
FROM ubuntu:latest as ENV
COPY --from=BASE /app/chigusa/target/release/chigusa /app/chigusa
ENTRYPOINT [ "/app/chigusa" ]
