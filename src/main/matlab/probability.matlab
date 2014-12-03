function n = toNumeric(s) 
	switch s
		case 'A'
			n = 1;
		case 'C'
			n = 2;
		case 'G'
			n = 3;
		case 'T'
			n = 4;
		otherwise
			n = 0;
	end
end

function c = probability(strings_, profile_)
	for i = 1:size(strings_, 1)
		c(i,1) = 1;
		for j = 1:size(strings_, 2)
			c(i,1) = c(i,1) * profile_(toNumeric(strings_(i,j)),j);
		end
	end
end
