#!/usr/bin/env python
#
# from https://abirchakraborty.wordpress.com/2015/07/13/model-building-in-vowpal-wabbit/

raw_file = 'housing.data'
col_names = ['CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT']
out_file = 'housing_vw.data'  # inputfile for VW
fw = open(out_file, 'w')
with open(raw_file, 'r') as finp:
    for line in finp:
        e = line.strip().split(' ')
        e = [w for w in e if len(w) > 0]
        out_str = e[-1] + ' | '
        for (feature,name) in zip(e,col_names):
            if float(feature) != 0:
                out_str += name + ':' + feature + ' '
        fw.write(out_str + '\n')
fw.close()
