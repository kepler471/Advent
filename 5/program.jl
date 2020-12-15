const input = split(read("input.txt", String))


seatID(row, col) = 8 * (row - 1) + (col - 1)


function locate(ticket, row=collect(1:128), col=collect(1:8))
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
	for i = X[2:end]
	   if tickets[i] - tickets[i-1] > 1
		   return tickets[i] - 1
	   end
	end
end


tickets = map(locate, input) |> sort
maxID = tickets[end]
missingID = findgap(tickets)

print("Largest seat ID: ", maxID, "\t:::\tMissing seat ID: ", missingID, "\n")

# Alternate 
# map(locate, input)
# locate.(input)
# [locate(i) for i in input]