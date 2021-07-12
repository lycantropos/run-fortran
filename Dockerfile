ARG IMAGE_NAME
ARG IMAGE_VERSION

FROM ${IMAGE_NAME}:${IMAGE_VERSION}

RUN pip install --upgrade pip setuptools

WORKDIR /opt/run-fortran

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY README.md .
COPY setup.py .
COPY run_fortran run_fortran

RUN pip install -e .

ENTRYPOINT ["run-fortran"]
