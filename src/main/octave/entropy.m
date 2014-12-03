function e = entropy(data)
	for i = 1:size(data,1)
		e(i) = 0;
		for j = 1:size(data,2)
			if (data(i,j) != 0)
				e(i) = e(i) - (data(i,j) * log2(data(i,j)));
			end
		end
	end
	e = e';
end