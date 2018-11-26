%Read the data
E = csvread("./example2.dat")
k = 4

%E = csvread("./example1.dat")
%k = 2

%Get the vertexs to column 1 and 2
col1 = E(:,1);
col2 = E(:,2);

%Get the max ids from the columns
max_ids = max(max(col1,col2));

%Step 1; Create afinite matrix A
As= sparse(col1, col2, 1, max_ids, max_ids); 
A = full(As)

%https://se.mathworks.com/help/matlab/ref/graph.plot.html
%print affinity matrix to see the amount of modules in the graph
%From this we can see that example 1 has 4 modules and eample2.dat has 2
%modules 
G = graph(A,'omitselfloops')
plot(G)


%returns diagonal matrix D of eigenvalues and matrix V whose columns are the corresponding right eigenvectors, so that A*V = V*D.
[v,D] = eig(A)

%Sort the eigenvalues
sort(diag(D))

%Step 2
L = (D^(-0.5))*A*(D^(-0.5))

%step 3 http://matlab.izmiran.ru/help/techdoc/ref/eigs.html
X = eigs(A,k)

%Step 4 renomrmalize X to Y
%https://se.mathworks.com/help/matlab/matlab_prog/array-vs-matrix-operations.html
%. for element wise operations
%S = sum(A,dim) returns the sum along dimension dim. For example, if A is a matrix, then sum(A,2) is a column vector containing the sum of each row.
%https://se.mathworks.com/help/matlab/ref/sum.html
Y = X./sqrt(sum(X.^2,2))

%https://se.mathworks.com/help/stats/kmeans.html
%step 5 use k-means in matlab
idx = kmeans(Y,k)

%Step 6
%orignal points in A
%



