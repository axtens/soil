'SOIL: Stack Oriented Interpreted Language
option explicit

class stack
	dim tos
	dim astack()
	dim stacksize
	
	private sub class_initialize
		stacksize = 1000
		redim astack( stacksize )
		tos = 0
	end sub

	public sub push( x )
		astack(tos) = x
		tos = tos + 1
	end sub
	
	public property get stackempty
		stackempty = ( tos = 0 )
	end property
	
	public property get stackfull
		stackfull = ( tos > stacksize )
	end property
	
	public property get stackroom
		stackroom = stacksize - tos
	end property
	
	public property get stackcount
		stackcount = tos + 1
	end property
	
	public function pop()
		if tos > 0 then
			pop = astack( tos - 1 )
			tos = tos - 1
		else
			wscript.echo "Error: 'pop' but not enough data on stack"
			'~ wscript.quit
		end if
	end function

	public sub resizestack( n )
		redim preserve astack( n )
		stacksize = n
		if tos > stacksize then
			tos = stacksize
		end if
	end sub
	
	public sub rotate
		dim last, i
		dim base
		base = tos - 1
		last = astack( base )
		for i = 1 to base - 1
			astack( i ) = astack( i - 1 )
		next
		astack( 0 ) = last
	end sub
	
	public sub swap
		dim tmp
		if stackcount > 1 then
			tmp = astack( 0 )
			astack( 0 ) = astack( 1 )
			astack( 1 ) = tmp 
		else
			wscript.echo "Error: 'swap' but not enough data on stack"
		end if
	end sub
	
	public sub dup
		dim tmp 
		if stackcount > 0 then
			tmp = pop
			push tmp
			push tmp
		else
			wscript.echo "Error: 'dup' but not enough data on stack"
		end if
	end sub
	
	public sub show
		dim i
		wscript.stdout.write "--["
		for i = 0 to tos - 1
			wscript.stdout.write astack( i )
			if i < tos - 1 then
				wscript.stdout.write ", "
			end if
		next
		wscript.echo "]--"
	end sub 
	
	public property get stack
		stack = join( aStack, ", " )
	end property
	
end class

class machine
	private ip
	private script
	private finished
	private blocks
	private subros
	private done
	
	private dict 
	
	sub class_initialize
		set blocks = createobject("scripting.dictionary")
		set dict = createobject("scripting.dictionary")
		done = false
		define "(", -1, ""
		define "+", -1, ""
		define "-", -1, ""
		define "gt", -1, ""
		define "lt", -1, ""
		define "eq", -1, ""
		define "ne", -1, ""
		define "and", -1, ""
		define "dup" , -1, ""
		define "rot" , -1, ""
		define "drop" , -1, ""
		define "execif" , -1, ""
		define "exec" , -1, ""
		define "clear" , -1, ""
		define "not" , -1, ""
		define "callstart" , -1, ""
		define "return" , -1, ""
		define "swap" , -1, ""
		define "CS>RS" , -1, ""
		define "RS>CS" , -1, ""
		define "1" , -1, ""
		define "0" , -1, ""
		define "print", -1, ""
		define "quit", -1, ""
		define "jumpstart" , -1, ""
	end sub
	
	public sub load( key, data )
		dim ss
		ss = replace( data, vbnewline, " " )
		ss = replace( ss, "  "," " )

		if blocks.exists( key ) then
			blocks(key) = split( ss, " " )
		else
			blocks.add key, split( ss, " " )
		end if
	end sub
	
	public function code( key )
		if blocks.exists( key ) then
			code = blocks(key)
		else
			code = array()
		end if
	end function
	
	public function define( sName, bPrimitive, sValue)
		if dict.exists( sName ) then
			dict(sName) = Array( bPrimitive, Split( sValue, " " ) )
		else
			dict.add sName, Array( bPrimitive, Split( sValue, " " ) )
		end if
	end function
	
	public function defType( sName )
		if dict.exists( sName ) then
			defType = dict( sName )( 0 )
		else
			defType = -2
		end if
	end function
	
	public function defValue( sName )
		if dict.exists( sName ) then
			defValue = dict( sName )( 1 )
		else
			defValue = Array()
		end if
	end function
	
	public property get nextop
		'~ wscript.stdout.write "(IP=" & ip & ") " & script(ip) & " "
		nextop = script( ip )'mid( script, ip, 1 )
		ip = ip + 1
		if ip > ubound( script ) then
			finished = true
		end if
	end property
	
	public sub prevop
		ip = ip - 1
		if ip = -1 then ip = 0
	end sub
	
	public property get isfinished
		isfinished = finished
	end property
	
	public sub firstop
		ip = 0
	end sub
	
	public function evaluate( blockname, startpoint )
		dim aCode
		dim ip
		dim c
		dim macsub
		dim ques
		dim context
		set context = new stack
		dim thing
		set thing = new stack
		dim env
		dim res 
		dim dType
		dim dictEntry
		
		aCode = code( blockname )
		ip = startpoint
		do while ip <= ubound( aCode ) and not done
			c = aCode( ip )
			'~ wscript.stdout.write c
			
			dType = defType( c )
			wscript.echo ip,": [",c,"]"," defType",defTypeName( dType )," --> ", join( defValue( c ), ", ")
			
			select case dType
			case -1 ' primitive
				select case c
				case "("
					do while aCode( ip ) <> ")"
						ip = ip + 1
					loop
				case "+"
					apply "+", 2
				case "-"
					apply "-", 2
				case "gt"
					apply ">", 2
				case "lt"
					apply "<", 2
				case "eq"
					apply "=", 2
				case "ne"
					apply "<>", 2
				case "and"
					apply "and", 2
				case "dup" 'dup
					CS.dup
				case "rot" 'rotate top n elements of stack
					CS.rotate
				case "drop" ' drop
					CS.pop
				'~ case "execif" 'test top of stack. Next op should be lowercase a..z being macro
					'~ ques = cs.pop
					'~ if ques then
						'~ macsub = aCode( ip + 1 )
						'~ wscript.stdout.write " " & macsub & " "
						'~ context.push array( blockname, ip )
						'~ res = evaluate( macsub, 0 )
						'~ env = context.pop
						'~ aCode = code( env( 0 ) )
						'~ ip = env(1)
					'~ end if
					'~ ip = ip + 1
				'~ case "exec" 'Next op should be lowercase a..z being mac
					'~ macsub = aCode( ip + 1 )
					'~ wscript.stdout.write " " & macsub & " " 
					'~ context.push array( blockname, ip )
					'~ res = evaluate( macsub, 0 )
					'~ env = context.pop
					'~ aCode = code( env( 0 ) )
					'~ ip = env(1)
					'~ ip = ip + 1
				'~ case "clear" 'Next op should be lowercase a..z being macro
					'~ macsub = aCode( ip + 1 )
					'~ wscript.stdout.write " " & macsub & " " 
					'~ m.load macsub, ""
				case "not" ' not
					if CS.stackcount > 0 then
						CS.push ( not CS.pop )
					else
						wscript.echo "Error: '~' but not enough data on stack"
					end if
				case "callstart" 'call
					'~ wscript.echo "%%" & ip & "%%"
					RS.push ip
					ip = 0
				case "return" 'return
					if RS.stackcount > 0 then
						ip = RS.pop
					else
						ip = -1  'ubound( aCode )
					end if
				case "swap" 'swap
					CS.swap
				case "CS>RS" 'pop from CS and push to RS
					IF CS.stackcount > 0 then
						tmp = CS.pop
						RS.push tmp 
					else
						wscript.echo "Error: '}' but not enough data on stack"
					end if
				case "RS>CS" 'pop from RS and push to CS
					IF RS.stackcount > 0 then
						tmp = RS.pop
						CS.push tmp
						'~ wscript.echo tmp,"pushed to program stack"
					else
						wscript.echo "Error: '}' but not enough data on return-stack"
					end if
				case "1" 'push 1 on stack
					CS.Push 1
				case "0" 'push 0 on stack
					CS.push 0
				case "print"
					wscript.echo CS.pop
				case "quit"
					wscript.quit 'exit do
				case "jumpstart" 'ip = 1
					m.firstop
				case else
					CS.push c
				end select
				
			case 0 ' composite
				env = array( blockname, ip )
				wscript.echo "storing env",join(env,", " )
				thing.push env
				blockname = c
				aCode = defValue( c )
				ip = -1
			
			case -2 ' error
				wscript.echo c,"unknown"
				wscript.quit
			end select
			
			CS.show
			
			if ip = -1 then 
				res = CS.pop
				done = true
			end if
			
			if ip >= ubound( aCode ) then
				env = thing.pop
				wscript.echo "recovering env",join(env,", ")
				blockname = env( 0 )
				aCode = code( blockname )
				ip = env( 1 )
				wscript.echo join( aCode, ", " ), ip
			end if
			
			ip = ip + 1
		loop	
		evaluate = res
	end function
	
	private sub apply( op, count )
		if CS.stackcount > count then
			'~ wscript.echo "[" & op & "]"
			 CS.push Eval( "CS.pop " & op & " CS.pop" )
		else
			wscript.echo "Error in Apply: '" & op & "' but not enough data on stack"
		end if
	end sub
	
	private function defTypeName( n )
		select case n
		case -1 
			defTypeName = "PRIMITIVE"
		case 0 
			defTypeName = "COMPOSITE"
		case -2 
			defTypeName = "UNKNOWN"
		end select
	end function
	
end class 


dim CS
dim RS

set CS = new stack
set RS = new stack

CS.push 0 'm
CS.push 1 'n
RS.push -1
CS.show

dim m
set m = new machine
'~ m.macro "save", "dup CS>RS swap dup CS>RS swap"
'~ m.load "save", "dup rot swap dup rot swap"
'~ m.load "test1", "drop 0 eq"
'~ m.load "test2", "swap drop 0 eq"
'~ m.load "main", "exec save exec test1 execif BLOCK1 exec save exec test2 execif BLOCK2 exec save exec BLOCK3" ' !z!d?A{{!z!e?B{{!z!C*"
'~ m.load "BLOCK1", "swap drop 1 + return"
'~ m.load "BLOCK2", "drop 1 + 1 callstart return"
'~ m.load "BLOCK3", "swap dup rot dup rot swap 1 swap - callstart swap 1 - swap callstart return"

m.define "0=",0,"dup 0 eq"
m.define "two",0,"1 1 +"
m.define "four",0,"two two +"
m.load "main", "four"
wscript.echo "main=",m.evaluate( "main", 0 )
'~ m.showscript