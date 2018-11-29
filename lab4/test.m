

%File one
%E = csvread("./example1.dat")
%k = 4

%File 2
%E = csvread("./example2.dat");
%k = 2;

%Validation graph
E = csvread("./test.dat")
k = 2

%Get the vertexs to column 1 and 2
col1 = E(:,1);
col2 = E(:,2);

%Get the max ids from the columns
max_ids = max(max(col1,col2));

%Step 1; Create afinite matrix A
As= sparse(col1, col2, 1, max_ids, max_ids);
A = full(As);

%https://se.mathworks.com/help/matlab/ref/graph.plot.html
%print affinity matrix to see the amount of modules in the graph
%From this we can see that example1.dat has 4 modules and eample2.dat has 2
%modules 
figure(1),
hold on;
G = graph(A);
plot(G)
hold off;
title('Graph');

%Get the laplacian of G so we can see what the K should be by checking
%minimum eigenvalues and the field Vector
O = laplacian(G);
[F,EigVal] = eigs(O,6,'SA');
diag(EigVal)
%From ploting the minimum Eigenvalues for example1 we see that 4 values are
%0 which means that we have 4 partions.
figure(2),
hold on;
plot(diag(EigVal),'ko')
hold off;
title('Graph');
%From printing the EigenVector we also see that there are 4/2 groups
figure(3),
hold on;
plot(sort(F(:,2)))
hold off;
title('Graph');

%Step 2
%Get the sum from the diagnol and create the matrix L
D = diag(sum(A,2));
L = (D^(-0.5))*A*(D^(-0.5));



%step 3 http://matlab.izmiran.ru/help/techdoc/ref/eigs.html
%Get the k largest eigenvectors
[eVec,vals] = eigs(L,size(L,1),'LM');
X = eVec(:,1:k);

%Step 4 renomrmalize X to Y
%https://se.mathworks.com/help/matlab/matlab_prog/array-vs-matrix-operations.html
%. for element wise operations
%S = sum(A,dim) returns the sum along dimension dim. For example, if A is a matrix, then sum(A,2) is a column vector containing the sum of each row.
%https://se.mathworks.com/help/matlab/ref/sum.html
Y = X./sqrt(sum(X.^2,2));

%https://se.mathworks.com/help/stats/kmeans.html
%step 5 use k-means in matlab
Idx = kmeans(Y,k);

%Step 6
%orignal points in A
% if row i in Y was assinged to 
% https://se.mathworks.com/help/matlab/ref/hold.html hold of
% https://se.mathworks.com/help/matlab/ref/figure.html ploting points

figure(4),
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
ylim([0 5])
hold off;
title('Test graph');
grid on;shg





