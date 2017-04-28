FROM python:3

WORKDIR /run-fortran
COPY . /run-fortran/
RUN python3 -m pip install .

ENTRYPOINT ["python3", "run-fortran.py"]
