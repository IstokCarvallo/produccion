$PBExportHeader$uo_manejoimpresora.sru
forward
global type uo_manejoimpresora from nonvisualobject
end type
end forward

global type uo_manejoimpresora from nonvisualobject
end type
global uo_manejoimpresora uo_manejoimpresora

type variables
Datastore 	ids_impresoras
String		is_impresoraactual, is_impresoracomp
end variables

forward prototypes
public function integer setimprcomp ()
public function integer setimprdef ()
public function boolean asignaimpresora ()
public function boolean asignaimpresora_comp (string as_impresoracomp)
end prototypes

public function integer setimprcomp ();Return PrintSetPrinter(is_impresoracomp)
end function

public function integer setimprdef ();Return PrintSetPrinter(is_impresoraactual)
end function

public function boolean asignaimpresora ();String 	ls_impresoras, ls_variable, ls_caracter, ls_ImpresoraActual, ls_ImpresoraComp
Integer	li_fila, li_posi, li_columna, li_row

ls_impresoras 		= 	PrintGetPrinters ( )
is_impresoraactual=	PrintGetPrinter ( )

DO WHILE Len(ls_impresoras) > 0
	li_columna		=	1
	li_posi			=	Pos(ls_impresoras, '~n')
	
	IF li_posi = 0 THEN
		ls_impresoras	=	ls_impresoras + '~n'
		li_posi	 		=	Pos(ls_impresoras, '~n')
	END IF
	
	FOR li_fila = 1 TO li_posi
		ls_caracter	=	Mid(ls_impresoras, li_fila, 1)
		
		IF ls_caracter <> '~t' AND ls_caracter <> '~n' THEN
			ls_variable = 	ls_variable + ls_caracter
		ELSE
			CHOOSE CASE li_columna
				CASE 1
					li_row												=	ids_impresoras.InsertRow(0)
					ids_impresoras.Object.impresora[li_row] 	= 	ls_variable
					
				CASE 2
					ids_impresoras.Object.drivers[li_row] 		= 	ls_variable
					
				CASE 3
					ids_impresoras.Object.puerto[li_row] 		= 	ls_variable
					
			END CHOOSE
			li_columna 		++
			ls_variable 	= 	''
			
		END IF
		
	NEXT
	ls_impresoras	=	Right(ls_impresoras, (Len(ls_impresoras) - li_posi))
	
LOOP

ls_ImpresoraComp	=	""//gstr_ParamPlanta.imprcompdef

li_row 				= 	ids_impresoras.Find("impresora = '"+ ls_ImpresoraComp + "'", 1, ids_impresoras.RowCount())
IF li_row = 0 THEN
	MessageBox("Error", "La impresora predeterminada para compactos no se encuentra instalada", StopSign!)
	Return False
END IF
is_impresoracomp	=	ids_impresoras.Object.impresora[li_row] + '~t' + &
							ids_impresoras.Object.drivers[li_row] + '~t' + &
							ids_impresoras.Object.puerto[li_row]
end function

public function boolean asignaimpresora_comp (string as_impresoracomp);String 	ls_impresoras, ls_variable, ls_caracter, ls_ImpresoraActual, ls_ImpresoraComp
Integer	li_fila, li_posi, li_columna, li_row

ls_impresoras 		= 	PrintGetPrinters ( )
is_impresoraactual=	PrintGetPrinter ( )
ls_ImpresoraComp	=	as_impresoracomp

DO WHILE Len(ls_impresoras) > 0
	li_columna		=	1
	li_posi			=	Pos(ls_impresoras, '~n')
	
	IF li_posi = 0 THEN
		ls_impresoras	=	ls_impresoras + '~n'
		li_posi	 		=	Pos(ls_impresoras, '~n')
	END IF
	
	FOR li_fila = 1 TO li_posi
		ls_caracter	=	Mid(ls_impresoras, li_fila, 1)
		
		IF ls_caracter <> '~t' AND ls_caracter <> '~n' THEN
			ls_variable = 	ls_variable + ls_caracter
		ELSE
			CHOOSE CASE li_columna
				CASE 1
					li_row												=	ids_impresoras.InsertRow(0)
					ids_impresoras.Object.impresora[li_row] 	= 	ls_variable
					
				CASE 2
					ids_impresoras.Object.drivers[li_row] 		= 	ls_variable
					
				CASE 3
					ids_impresoras.Object.puerto[li_row] 		= 	ls_variable
					
			END CHOOSE
			li_columna 		++
			ls_variable 	= 	''
			
		END IF
		
	NEXT
	ls_impresoras	=	Right(ls_impresoras, (Len(ls_impresoras) - li_posi))
	
LOOP

//ls_ImpresoraComp	=	gstr_ParamPlanta.imprcompdef

li_row 				= 	ids_impresoras.Find("impresora = '"+ ls_ImpresoraComp + "'", 1, ids_impresoras.RowCount())
IF li_row = 0 THEN
	MessageBox("Error", "La impresora predeterminada para compactos no se encuentra instalada", StopSign!)
	Return False
END IF
is_impresoracomp	=	ids_impresoras.Object.impresora[li_row] + '~t' + &
							ids_impresoras.Object.drivers[li_row] + '~t' + &
							ids_impresoras.Object.puerto[li_row]
end function

event constructor;ids_impresoras					=	Create DataStore
ids_impresoras.DataObject	=	'dw_ctrol_printers'
//AsignaImpresora()
end event

on uo_manejoimpresora.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_manejoimpresora.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

