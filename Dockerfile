FROM ponylang/ponyc:0.25.0
COPY basicc basicc
RUN ponyc -o /bin basicc
CMD ["basicc", "--help"]
