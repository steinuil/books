print_table = (a) ->
  for k,v in pairs a
    print "#{k} => #{v}"
  print "------"

insert_sort = (a) ->
  for j = 2, #a
    key = a[j]
    i = j - 1
    while i > 0 and a[i] > key
      a[i + 1] = a[i]
      i = i - 1
    a[i + 1] = key
  return a

min = (a) ->
  last = 1
  for k,v in pairs a
    if v < a[last]
      last = k
  return last

select_sort = (a) ->
  for j = 1, #a - 1
    slice = [i for i in *a[j,]]
    print_table slice
    i = min(slice) + (j - 1)
    tmp = a[j]
    a[j] = a[i]
    a[i] = tmp
  return a

merge = (a, q) ->
  left = [i for i in *a[1,p]]
  right = [i for i in *a[p + 1,]]
  
  i = 1
  j = 1
  for k = 1, #a
    if left[i] <= right[j]
      a[k] = left[i]
      i = i + 1
    else
      a[k] = right[j]
      j = j + 1
  return a

a = {5, 2, 4, 6, 1, 3}

print_table(select_sort a)
