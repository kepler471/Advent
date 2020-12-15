const input = rstrip(read("input.txt", String), '\n')

yescount = map(x -> replace(x, "\n" => "") |> Set |> length, split(input, "\n\n")) |> sum
yescountall = map(x -> reduce(∩, split(x, "\n")) |> length, split(input, "\n\n")) |> sum

#= Notes and alternate methods
#Part 1.
# Time winner
map(x -> replace(x, "\n" => "") |> Set |> length, split(input, "\n\n")) |> sum

# Space winner
function nunique(a)
	last = first(a)
	n = 1
	for x in a
		if isless(last, x)
			n += 1
			last = x
		end
	end
	n
end
map(x -> replace(x, "\n" => "") |> collect |> sort |> nunique |> length, split(input, "\n\n")) |> sum

# Double loser
map(x -> replace(x, "\n" => "") |> unique |> length, split(input, "\n\n")) |> sum

# Alternate (StackOverflow answer for parallelised version)
function nunique(v)
    @assert length(v) > 0
    cnt = 1
    lasta::eltype(v) = first(v)
    @inbounds for newa1 in v
        if !isequal(newa1, lasta)
            cnt += 1
            lasta = newa1
        end
    end
    cnt
end

""" Parallelised version """
function pnunique(v)
    nt = Threads.nthreads()
    lo::Vector{Int} = collect(0:div(length(v), nt):length(v)-1)
    hi::Vector{Int} = lo[2:end]
    hi[end] = length(v)
    lo = lo .+ 1

    nu = Vector{Int}(undef, nt)

    Threads.@threads for i in 1:nt
        @inbounds nu[i] = nunique(@view v[lo[i]:hi[i]])
    end

    res = sum(nu)
    for j in 1:nt-1
        @inbounds res -= v[hi[j]] == v[lo[j+1]]
    end

    res
end

Part 2.
# This has nested mapping which is awkward
map(y -> reduce(∩, y) |> length, map(x -> split(x, "\n"), split(input, "\n\n"))) |> sum
# Fixed by composition of reduce and split
map(x -> reduce(∩, split(x, "\n")) |> length, split(input, "\n\n")) |> sum
=#
