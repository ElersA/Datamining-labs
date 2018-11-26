%Read the data
%E = csvread("./example2.dat");
%k = 2;

E = csvread("./example1.dat")
k = 4

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
%From this we can see that example1.dat has 4 modules and eample2.dat has 2
%modules 
G = graph(A,'omitselfloops');
plot(G)

f = [2,3;4,1]

D = diag(sum(A,2));


%Step 2
L = (D^(-0.5))*A*(D^(-0.5));

%step 3 http://matlab.izmiran.ru/help/techdoc/ref/eigs.html
%[Svec,Fval] = eig(L);
[X,S] = eigs(L,k)


%Step 4 renomrmalize X to Y
%https://se.mathworks.com/help/matlab/matlab_prog/array-vs-matrix-operations.html
%. for element wise operations
%S = sum(A,dim) returns the sum along dimension dim. For example, if A is a matrix, then sum(A,2) is a column vector containing the sum of each row.
%https://se.mathworks.com/help/matlab/ref/sum.html
Y = X./sqrt(sum(X.^2,2))

%https://se.mathworks.com/help/stats/kmeans.html
%step 5 use k-means in matlab
Idx = kmeans(Y,k)

%Step 6
%orignal points in A
% if row i in Y was assinged to 
% https://se.mathworks.com/help/matlab/ref/hold.html hold of
% https://se.mathworks.com/help/matlab/ref/figure.html ploting points

figure,
hold on;

for i=1:size(Idx,1)
    if Idx(i,1) == 1
        plot(i,1,'kx');
    elseif Idx(i,1) == 2
        plot(i,2,'kx');
    elseif Idx(i,1) == 3
        plot(i,3,'kx');   
    elseif Idx(i,1) == 4
        plot(i,4,'kx');   
    end
end
hold off;
title('Test graph');
grid on;shg





