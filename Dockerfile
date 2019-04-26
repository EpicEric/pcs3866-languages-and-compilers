FROM ponylang/ponyc:0.28.0
COPY basicc basicc
RUN ponyc --debug -o /bin basicc
CMD ["basicc", "--help"]
