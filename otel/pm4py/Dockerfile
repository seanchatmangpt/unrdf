FROM python:3.12-slim

LABEL maintainer="unrdf"
LABEL description="PM4Py process mining with OTEL integration"

WORKDIR /app

# Install system deps
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
    && rm -rf /var/lib/apt/lists/*

# Install Python deps
RUN pip install --no-cache-dir \
    pm4py \
    papermill \
    jupyter \
    requests \
    pandas \
    matplotlib

# Patch pm4py constants.py to handle PID 0 in Docker containers
RUN sed -i 's/parent_name = str(psutil.Process(parent_pid).name())/parent_name = "docker-container"/' \
    /usr/local/lib/python3.12/site-packages/pm4py/util/constants.py

# Copy analysis files
COPY pm4py-analyze.py /app/
COPY pm4py-process-mining.ipynb /app/

# Default: run standalone CLI analysis
ENTRYPOINT ["python3", "/app/pm4py-analyze.py"]
CMD ["--hours", "24", "--limit", "1000"]
