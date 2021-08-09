let rec sum n m f = 
        if n = m then (f m)
        else (f n) + (sum (n+1) m f)
