# -*- coding: utf-8 -*-

# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.express as px
import pandas as pd

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
df = pd.DataFrame({
    "Fruit": ["蘋果", "橘子", "香蕉", "蘋果", "橘子", "香蕉"],
    "Amount": [47, 15, 24, 28, 46, 52],
    "City": ["Taipei", "Taipei", "Taipei", "Taoyan", "Taoyan", "Taoyan"]
})
fig = px.bar(df, x="Fruit", y="Amount", color="City", barmode="group")

app.layout = html.Div(children=[
    html.H1(children='Hello Dash'),
    html.Div(children='''Dash: A web application framework for Python.'''),
    dcc.Graph(
        id='example-graph',
        figure=fig
    )
])

if __name__ == '__main__':
    app.run_server(debug=True)