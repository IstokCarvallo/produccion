$PBExportHeader$uo_dw.sru
forward
global type uo_dw from datawindow
end type
type col_struct from structure within uo_dw
end type
type validation_struct from structure within uo_dw
end type
end forward

type col_struct from structure
    integer tab
    string col
end type

type Validation_struct from structure
    string expression
    string error_message
end type

global type uo_dw from datawindow
integer width = 709
integer height = 364
integer taborder = 1
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
event add_row pbm_custom20
event del_row pbm_custom19
event del_all_rows pbm_custom18
event on_delete pbm_custom17
event on_insert pbm_custom16
event on_update pbm_custom15
event ue_deshace pbm_custom44
event ue_columnacambia pbm_custom59
event dwnkey pbm_dwnkey
event ue_seteafila pbm_custom21
event ue_nomover pbm_syscommand
end type
global uo_dw uo_dw

type variables
Boolean				ib_allow_updates	=	True, ib_allow_inserts	=	True
Long					il_selected_row
DataStore			ids_CtlRep
DateTime				GrupoFecha

Private:
validation_struct istr_validations[]
end variables

forward prototypes
public subroutine uf_add_validation (string expression, string error_message)
public function boolean uf_validate (long row)
public function boolean uf_is_modified ()
public function boolean uf_check_required (integer num_dw)
end prototypes

event ue_nomover;uint wParam, lParam

wParam = Message.WordParm

Choose Case wParam
	Case 61456, 61458
	     Message.Processed = True
	     Message.ReturnValue = 0

End Choose
end event

public subroutine uf_add_validation (string expression, string error_message);int cnt

cnt = upperbound(istr_validations) + 1

istr_validations[cnt].expression = expression

istr_validations[cnt].error_message = error_message
end subroutine

public function boolean uf_validate (long row);window current_win
int expr,num_expr
string error_msg,result
boolean one_time
num_expr = upperbound(istr_validations)
if num_expr = 0 then return true // if no rules then get out right away
if this.accepttext() = -1 then return false
error_msg = ""
if row <> 0 then
	one_time = true
else
	one_time = false
	row = this.getnextmodified(0,primary!)
end if
do while row > 0 
	for expr = 1 to num_expr
		result =  this.describe("evaluate('"+istr_validations[expr].expression+"',"+string(row)+")")
		if result <> 'true' then
			error_msg = istr_validations[expr].error_message
			Exit
		END IF
	next
	if len(error_msg) > 0 then // there is an error
		this.setrow(row)
		this.scrolltorow(row)
		current_win = parent // get the current window title for the error message
		messagebox(current_win.title+" en la fila "+string(row),error_msg)
		this.setfocus()
		return false
	end if
	// get the next modified row
	if one_time then
		exit
	else 
		row = this.getnextmodified(row,primary!)
	end if
loop
// if we get to here then there are no errors
return true
end function

public function boolean uf_is_modified ();// return true if there are any modified or deleted rows
// or if the dataindow does not pass validations
if this.accepttext() = -1 then return true
return this.modifiedcount() > 0 or this.deletedcount() > 0 
end function

public function boolean uf_check_required (integer num_dw);window parent_win
integer	col
string	colname

message.doubleparm = 0

if num_dw = 0 then
	parent.triggerevent("ue_requerido")
else
	parent.triggerevent("ue_detalle_requerido")
end if

col = message.doubleparm
if col <> 0 then
	parent_win = parent
	colname = this.Describe("#"+string(col)+".Name")
   	MessageBox(parent_win.title,"Se requiere "+ upper(colname) +" en el registro No." + string (getrow())+'. Por favor, ingrese un valor.',stopsign! )
	this.SetColumn(col)
	this.setfocus()
	return false
end if

return true
end function

event clicked;String	Tab

IF Row <= 0 THEN RETURN

IF Row <> This.GetRow() THEN
	Tab	=	Describe('#' + String(dwo) + '.tabsequence')
	
	IF Tab = '0' THEN This.SetRow(Row)
END IF
end event

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

event itemerror;String	s

s	=	This.Describe(This.GetColumnName()+".coltype")

CHOOSE CASE s
	CASE "number"
		IF Trim(This.GetText())= "" OR Not IsNumber(Trim(This.GetText())) THEN
			Int	null_num
			
			SetNull(null_num)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_num)
			
			RETURN 3
		END IF
		
	CASE "date"
		IF Trim(This.GetText()) = "" THEN
			Date	null_date
			
			SetNull(null_date)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_date)
			
			RETURN 3
		END IF
		
	CASE "time"
		IF Trim(This.GetText()) = "" THEN
			Time	null_time
			
			SetNull(null_time)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_time)
			
			RETURN 3
		END IF
		
	CASE "datetime"
		IF Trim(This.GetText()) = "" THEN
			Date	null_datetime
			
			SetNull(null_datetime)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_datetime)
			
			RETURN 3
		END IF
		
	CASE ELSE
		RETURN 3
		
END CHOOSE
end event

event sqlpreview;DwItemStatus	Estado
Long				ll_Fila
String			ls_Tabla, ls_Sentencia1, ls_Sentencia2, ls_Sentencia3, &
					ls_Sentencia4, ls_Sentencia5
DateTime			ldt_FechaHora
Date				ld_Fecha
Time				lt_Hora

IF Row = 0 THEN RETURN 0

//MessageBox("SqlPreview", "Se gatilló SQLPreview con~r~r" + sqlsyntax)

ls_Sentencia1	=	SqlSyntax

IF Len(ls_Sentencia1) >= 255 THEN
	ls_Sentencia2	=	Mid(ls_Sentencia1, 256)
	ls_Sentencia1	=	Mid(ls_Sentencia1, 1, 255)
END IF

IF Len(ls_Sentencia2) >= 255 THEN
	ls_Sentencia3	=	Mid(ls_Sentencia2, 256)
	ls_Sentencia2	=	Mid(ls_Sentencia2, 1, 255)
END IF

IF Len(ls_Sentencia3) >= 255 THEN
	ls_Sentencia4	=	Mid(ls_Sentencia3, 256)
	ls_Sentencia3	=	Mid(ls_Sentencia3, 1, 255)
END IF

IF Len(ls_Sentencia4) >= 255 THEN
	ls_Sentencia5	=	Mid(ls_Sentencia4, 1, 255)
	ls_Sentencia4	=	Mid(ls_Sentencia4, 256)
END IF

IF Len(ls_Sentencia5) >= 255 THEN
	ls_Sentencia5	=	Mid(ls_Sentencia5, 1, 255)
END IF

ls_Tabla											=	Mid(ls_Sentencia1, Pos(Upper(ls_Sentencia1), ' "') + 1)
ls_Tabla											=	Mid(ls_Tabla, 1, Pos(Upper(ls_Tabla), '" '))
ls_Tabla											=	F_Global_Replace(ls_Tabla, '"', '')
ldt_FechaHora									=	F_FechaHora()
ld_Fecha											=	Date(ldt_FechaHora)
lt_Hora											=	Time(ldt_FechaHora)
ll_Fila											=	ids_CtlRep.InsertRow(0)
ids_CtlRep.Object.sist_codigo[ll_Fila]	=	gstr_apl.CodigoSistema
ids_CtlRep.Object.lotr_aplica[ll_Fila]	=	Parent.ClassName()
ids_CtlRep.Object.tare_tabrep[ll_Fila]	=	ls_Tabla
ids_CtlRep.Object.lotr_fechat[ll_Fila]	=	ld_Fecha
ids_CtlRep.Object.lotr_horatr[ll_Fila]	=	lt_Hora
ids_CtlRep.Object.lotr_sente1[ll_Fila]	=	ls_Sentencia1
ids_CtlRep.Object.lotr_sente2[ll_Fila]	=	ls_Sentencia2
ids_CtlRep.Object.lotr_sente3[ll_Fila]	=	ls_Sentencia3
ids_CtlRep.Object.lotr_sente4[ll_Fila]	=	ls_Sentencia4
ids_CtlRep.Object.lotr_sente5[ll_Fila]	=	ls_Sentencia5
ids_CtlRep.Object.lotr_usutra[ll_Fila]	=	gstr_Us.Nombre
ids_CtlRep.Object.lotr_comptr[ll_Fila]	=	gstr_Us.Computador
ld_Fecha											=	Date(GrupoFecha)
lt_Hora											=	Time(GrupoFecha)
ids_CtlRep.Object.lotr_fecgpo[ll_Fila]	=	ld_Fecha
ids_CtlRep.Object.lotr_horgpo[ll_Fila]	=	lt_Hora

CHOOSE CASE Buffer
	CASE Delete!
		This.TriggerEvent("on_delete")
		ids_CtlRep.Object.lotr_tipotr[ll_Fila]	=	3
		
	CASE Primary!, Filter!
		Estado	=	This.GetItemStatus(Row, 0, Buffer)
		
		CHOOSE CASE Estado
			CASE New!, NewModified!
				This.TriggerEvent("on_insert")
				ids_CtlRep.Object.lotr_tipotr[ll_Fila]	=	1

			CASE DataModified!
				This.TriggerEvent("on_update")
				ids_CtlRep.Object.lotr_tipotr[ll_Fila]	=	2

		END CHOOSE
END CHOOSE
	
IF Message.ReturnValue = 1 THEN
	RETURN 1
ELSE
	RETURN 0
END IF
end event

on uo_dw.create
end on

on uo_dw.destroy
end on

event updateend;IF RowsInserted + RowsUpdated + RowsDeleted > 0 THEN
	IF ids_CtlRep.Update(True, False) = 1 THEN 
//		Commit;
//		
//		IF it_Trans.SQLCode <> 0 THEN
//			F_ErrorBaseDatos(it_Trans, "Error en Control de Replicación")
//		ELSE
			ids_CtlRep.ResetUpdate()
//		END IF
	ELSE
//		RollBack;
//		
//		IF it_Trans.SQLCode <> 0 THEN F_ErrorBaseDatos(it_Trans, "Error en Control de Replicación")
	END IF

		ids_CtlRep.Reset()
END IF
end event

event constructor;ids_CtlRep	=	Create DataStore

ids_CtlRep.DataObject	=	"dw_mues_admalogtransacs"
end event

event updatestart;ids_CtlRep.SetTransObject(sqlca)
end event

