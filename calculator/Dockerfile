FROM rust:1.46 AS builder

WORKDIR /opt/src/calculator

COPY Cargo.toml .
COPY src/ src/

RUN cargo test --all \
&& cargo build --all --release

# ----

FROM debian:buster

COPY --from=builder /opt/src/calculator/target/release/main /opt/bin/calculator

CMD /opt/bin/calculator