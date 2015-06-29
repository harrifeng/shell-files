# One side
nc -l 5001 | pv -W > /dev/null

# The other side
dd if=/dev/zero bs=1MB count=100 |nc one-side-ip-hostname 5001
