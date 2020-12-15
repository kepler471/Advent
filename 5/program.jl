input = split(read("input.txt", String))


function seatID(row, col)
	8 * (row - 1) + (col - 1)
end


function locate(ticket, row::Array{Int,1}=collect(1:128), col::Array{Int,1}=collect(1:8))
	if length(ticket) == 0
		return seatID(row[1], col[1])
	elseif ticket[1] == 'F'
		row = row[1:div(end,2)]
	elseif ticket[1] == 'B'
		row = row[1+div(length(row),2):end]
	elseif ticket[1] == 'L'
		col = col[1:div(end,2)]
	elseif ticket[1] == 'R'
		col = col[1+div(length(col),2):end]
	end
	return locate(ticket[2:end], row, col)
end


function findgap(X)
	for i = X
	   if i > 1 && tickets[i] - tickets[i-1] > 1
		   return tickets[i] - 1
	   end
	end
end


tickets = sort(map(locate, input))
maxID = tickets[end]
missingID = findgap(tickets)

print("Largest seat ID: ", maxID, "\t:::\tMissing seat ID: ", missingID)