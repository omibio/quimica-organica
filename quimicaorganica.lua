iden = {
idenramci = function(str)
	cicleno = string.find(str, 'ciclo')
	ramificada,ram = string.find(str, 'il')
	return cicleno, ramificada, ram
end,

identrue = function(str, vp, ramificada)
	if vp == 'dec' then
		procurar = string.sub(str, 3, -1)
		dupla = string.find(procurar, "d")
	else
		dupla = string.find(str, "di")
	end
	if not dupla then
		dupla = 0
	end
	if ramificada then
		strr = string.sub(str, ram + 1)
		strr1,strr2 = string.find(str, strr)
		ram = string.sub(str,0, -strr2 - 2 + strr1)
		tram = string.sub(ram, 0, -#ram )
		tram2 = string.sub(ram, 2, -1)
		return dupla, ram, tram, tram2
	end
	return dupla
end}

function subidentificar(prefixo, ram, tram, tram2)
	for k,v in pairs(prefixo) do
		if string.find(tostring(ram), v) then
			ks, vs = k, v
			return ks, vs, ram , tram, tram2
		end
	end
end

function identificar(str)
	prefixo = {"met", "et", "prop", "but", "pent", "hex", "hept", "oct" , "non", "dec"}
	infixo = {"an", "en", "in"}
	iden.idenramci(str)
	iden.identrue(str, vp, ramificada)
	if ramificada then
		str = string.sub(str, #ram +1)
	end
	for kp, vp in pairs(prefixo) do
		for ki, vi in pairs(infixo) do
			findp = string.find(str, vp)
			findi = string.find(str, vi)
			keyp,vap = kp, vp
			keyi,vai = ki, vi
			if findp and findi then
				return keyp, str, keyi, vai, cicleno ~= nil, dupla, ram ~= nil, subidentificar(prefixo, ram, tram, tram2)
			end
		end
	end
end

function diminuir(tabela,y, keyi,l, i,keyp)
	if i == keyp then
		if y == i - 1 then
			if keyi == 2 then
				return 0
			elseif keyi == 3 then
				return 1
			end
		end
		return 0
	end
	if y == i then
		if i == 1 then
			return 0
		end
		if keyi == 2 then
			return 2
		elseif keyi == 3 then
			return 3
		else
			return 1
		end
		return 0
	end
	if l == 2*keyp-2 then
		if keyi == 1 then
			return 0
		elseif keyi == 2 then
			return 1
		elseif keyi == 3 then
			return 2
		end
	end
	if l == 2 then
		if keyi == 1 then
			return 0
		elseif keyi == 2 then
			return 1
		else
			return 1
		end
	elseif l > 2 and l < 2*keyp-2 then
		if keyi == 1 then
			return 1
		elseif keyi == 2 then
			return 1
		else
			return 2
		end
	end
	if keyi == 1 then
		return 0
	elseif keyi == 2 then
		return 1
	else
		return 0
	end
	if i == 1 then
		return 0
	end
end

function diminuir2(l,y,keyi,keyp)
	if l - 2 == y then
		return 2
	elseif l-2 ~= 0 and l-2 < 2*keyp-1 then
		if l-1 == y then
			return 1
		elseif 2*keyp - 2 ~= y then
			return 1
		else
			return 0
		end
	end
	if l == 2 then
		if keyi == 1 then
			return 1
		end
	end
	if l == 2 then
		if l == y*2 then
			if keyi == 2 then
				return 2
			else
				return 3
			end
		end
		return 0
	end
end

function ddiminuir(dupla, tabela, keyi,str,x)
	if dupla > 0 then
		if tabela[2] - tabela[1] == 2 then
			if i > 2 and i < 2*keyp -1 then
				if i+2 == tabela[1] and i+1 == tabela[2] then
					return 2
				end
			end
			if i + 1 == tabela[1] and i+1 == tabela[2] then
				if i > 1 then
					return 2
				end
				return 1
			end
			if i == tabela[1] then
				if i+2 == tabela[2] then
					if i + 1 == tabela[1] then
						return 2
					end
				end
				if i == 2 then
					return 2
				end
				return 1
			end
		end
		if i*2 == tabela[1] or tabela[2] == i*2 then
			if tabela[2] - tabela[1] == 2 then
				if i*2 == tabela[2] then
					return 2
				end
			end
			return 1
		end
		if i*2-2 == tabela[1] or i*2-2 == tabela[2] then
			if i*2 == tabela[1] or i*2 == tabela[2] then
				return 1
			end
			return 1
		end
		return 0
	end
	return 0
end

function dligacao(x,tabela,v)
	if keyi == 2 then
		x[tabela[v]] = '=='
	else
		x[tabela[v]] = '##'
	end
	return x[tabela[v]]
end

function ligacao(str, i, l, dupla, keyi,z,tabela)
	if dupla > 0 then
		if i*2 == tabela[1] or i*2 == tabela[2] then
			if i*2 == z then
				return dligacao(x,tabela,1)
			end
			return dligacao(x,tabela,2)
		end
	end
	if z == i*2 then
		if keyi == 2 then
			x[z] = '=='
		else
			x[z] = '##'
		end
		return x[z]
	end
	x[l] = '--'
	return x[l]
end

function ch(v, ks, x)
	x[v] = 'CH'.. 4 - diminuir(x,y, keyi,l, i,keyp) - diminuir2(l,y,keyi,keyp) - ddiminuir(dupla, tabelad, keyi,str,x) - rdiminuir(ks, i)
	if x[v] == 'CH1' then
		x[v] = 'CH'
	elseif x[v] == 'CH0' then
		x[v] = 'C'
	end
	return x[v]
end

function rdiminuir(ks, i)
	if ramificada then
		if i == tonumber(tram) then
			return 1
		end
		if not n then
			n = 0
		end
		if n ~= 0 then
			if n < ks*2 then
				return 2
			elseif n >= ks*2 then
				return 1
			end
		end
	end
	return 0
end

function getpostion(tram)
	tabela = {}
	p = 0
	soma = 0
	repeat
		p = p + 1
		if p%2 == 1 then
			soma = soma + 3
		else
			soma = soma + 2
		end
	until(not(p < tram*2))
	return soma -4
end

function rloop(ir, tram)
	while ir < getpostion(tram) do
		ir = ir + 1
		io.write(' ')
	end
end

function pramificacao(ramificada, ks, tram, tram2, d,i)
	if ramificada then
		tr = {}
		loc = tram*2
		print('')
		for z = 1, ks do
			if ir == nil then
				ir = 1
			end
			while z <= ks do
				z = ks + 1
				if not va then
					va = 1
				end
				if not ((ks*2-1)%2 == 0) then
					rloop(ir, tram)
					print(' |')
					va = va + 2
				end
				rloop(ir, tram)
				n = n + 2
				print('CH'.. 4 - rdiminuir(ks,i))
			end
		end
	end
end

function calcular(i, str, keyp, keyi,dupla)
	l = i + 1
	tabelad = {}
	x = {}
	if keyi > 1 and keyp > 3 and dupla == 0 then
		x1 = string.find(str,'-')
		x1 = string.sub(str, x1+1)
		y = string.find(x1,'-')
		y = tonumber(string.sub(x1, 0, -#x1 -2 +y))
		z = y*2
	elseif (keyp == 2 or keyp == 3 )and (dupla == 0 and keyi > 1) then
		y = 1
		z = 2*y
	elseif keyp == 3 and dupla > 0 and keyi == 2 then
		y = 1
		tabelad[1] = y*2
		y = 2
		tabelad[2] = y*2
		if i == 3 then
			return print('CH2==C==CH2')
		end
		return
	elseif keyp > 2 and keyi > 1 and dupla ~= 0 then
		x1,x2 = string.find(str, '-')
		yp = string.sub(str, x1 + 1, -#str + x2 + 2)
		y1 = string.find(yp, ',')
		y = string.sub(yp, 0,-y1 - 1)
		tabelad[1] = y*2
		y = string.sub(yp, y1 + 1)
		tabelad[2] = y*2
	end
	if keyp == 1 then
		return io.write('CH4')
	end
	if i == 1 then
		ch(1,ks, x)
		return io.write(x[1] .. ligacao(str,i,l,dupla,keyi,z,tabelad))
	elseif i > 1 and i < keyp then
		ch(l,ks, x)
		return io.write(x[l] .. ligacao(str,i,l,dupla,keyi,z,tabelad))
	else
		ch(l,ks, x)
		return io.write(x[l]), print(pramificacao(ramificada, ks, tram, tram2, d,i))
	end
end

function real(keyp, str, keyi,dupla)
	for z = 1, keyp do
		if i == nil then
			i = 0
		end
		i = i + 1
		calcular(i,str,keyp,keyi,dupla)
	end
end

input = io.read()
identificar(input)

print('\n')
real(keyp, input, keyi,dupla)
print('\n')










