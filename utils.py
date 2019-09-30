import pymysql
import pandas as pd
import io
from sqlalchemy import create_engine
import numpy as np
import datetime, sys
import warnings
import os, shutil
import dateutil.parser
import pytz

warnings.filterwarnings("ignore")

### data to parq
def data_to_parq(
    data,
    base_folder="/home/data/data_parq",
    version="v1",
    table_name="data",
    partition_cols=["dt"],
):
    path_save = os.path.join(
        base_folder, f"{table_name}", f"{table_name}" + "_" + f"{version}" + ".parquet"
    )

    if f"{table_name}" not in os.listdir(base_folder):
        os.mkdir(os.path.join(base_folder, table_name))

    ## if data.dt had been saved before, delete to avdoi append
    data_date = list("dt=" + data.dt.astype(str).unique())

    if f"{table_name}" + "_" + f"{version}" + ".parquet" in os.listdir(
        os.path.join(base_folder, f"{table_name}")
    ):
        date_save = os.listdir(path_save)
        data_drop = [x for x in data_date if x in date_save]
        if len(data_drop) > 0:
            print(f"{table_name} data in {data_drop} will replace")
            for tmp in data_drop:
                shutil.rmtree(os.path.join(f"{path_save}", tmp))

    data.to_parquet(path_save, partition_cols=partition_cols)
    print(f"{table_name} data have saved")


### utc isoformat string transform
def utc_isoformat_datetime_transform(utc_time):
    time_use = dateutil.parser.parse(utc_time)
    tz = pytz.timezone("Asia/shanghai")
    time_use = time_use.astimezone(tz).replace(tzinfo=None)
    return time_use


### get data parquet save path
def get_data_path(
    table_name="user_feedback_cnt",
    base_folder="/home/data/data_parq_fic/",
    version="v1",
):
    return os.path.join(base_folder, table_name, f"{table_name}_{version}.parquet")


### mysql database connection
class MySQLConnect:
    def __init__(
        self,
        host="localhost",
        port=3306,
        user="***",
        passwd="********",
        db="***",
        charset="utf8",
    ):
        try:
            self.host = host
            self.port = port
            self.user = user
            self.passwd = passwd
            self.db = db
            self.charset = charset
            self.Connect = pymysql.Connect(
                host=host, port=port, user=user, passwd=passwd, db=db, charset=charset
            )
            print("{}连接成功".format(self.Connect.db))

        except Exception as e:
            print("{}连接失败:{}".format(self.Connect.db, e))

    def pd_query(
        self,
        sql,
        index_col=None,
        coerce_float=True,
        params=None,
        parse_dates=None,
        columns=None,
        chunksize=None,
    ):
        return pd.read_sql(
            sql,
            self.Connect,
            index_col=index_col,
            coerce_float=coerce_float,
            params=params,
            parse_dates=parse_dates,
            columns=columns,
            chunksize=chunksize,
        )

    def exe_sql(self, *sql):
        try:
            cursor = self.Connect.cursor()
            cursor.execute(*sql)
            self.Connect.commit()
        except Exception as e:
            print(e)
            raise e
            self.Connect.rollback()

    # save pandas.DataFrame to MySQL, if exist, drop original table
    def df_to_sql(self, df, table_name, chunksize=None, if_exists="append", dtype=None):
        db_engine = create_engine(
            f"mysql+pymysql://{self.user}:{self.passwd}@{self.host}:{self.port}/{self.db}"
        )
        df.to_sql(
        table_name,
            db_engine,
            index=False,
            if_exists=if_exists,
            chunksize=chunksize,
            dtype=dtype,
        )

    def close(self):
        self.Connect.close()
