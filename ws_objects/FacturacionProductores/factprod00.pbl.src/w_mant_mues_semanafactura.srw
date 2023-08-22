$PBExportHeader$w_mant_mues_semanafactura.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_semanafactura from w_mant_directo
end type
end forward

global type w_mant_mues_semanafactura from w_mant_directo
integer width = 2327
integer height = 1900
string title = "MAESTRO SEMANAS FACTURACION"
end type
global w_mant_mues_semanafactura w_mant_mues_semanafactura

type variables
Integer	il_FilaAnc 
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	dw_1.Object.fase_numero[il_fila]

CHOOSE CASE columna
	CASE "fase_numero"
		li_codigo	=	Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("fase_numero = " + String(li_codigo), 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;buscar			= "Semana:Nfase_numero,,Fecha Inicio:Sfase_inicio"
ordenar			= "Semana:Nfase_numero,Fecha Inicio:fase_inicio"
is_ultimacol		= "fase_final"
end event

on w_mant_mues_semanafactura.create
call super::create
end on

on w_mant_mues_semanafactura.destroy
call super::destroy
end on

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.fase_numero[il_fila]) OR dw_1.Object.fase_numero[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 		= ls_mensaje + "~nCódigo de Estado"
	ls_colu[li_cont]	= "fase_numero"
END IF

IF Isnull(dw_1.Object.fase_inicio[il_fila]) THEN
	li_cont ++
	ls_mensaje 		= ls_mensaje + "~nFecha Inicio"
	ls_colu[li_cont]	= "fase_inicio"
END IF

IF Isnull(dw_1.Object.fase_final[il_fila]) THEN
	li_cont ++
	ls_mensaje 		= ls_mensaje + "~nFecha Final"
	ls_colu[li_cont]	= "fase_final"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO SEMANAS FACTURACION"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, lstr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_semanafactura
boolean visible = false
integer x = 82
integer y = 1344
integer width = 1541
integer height = 244
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_semanafactura
integer x = 1865
integer y = 336
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_semanafactura
integer x = 1865
integer y = 64
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_semanafactura
integer x = 1865
integer y = 720
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_semanafactura
integer x = 1865
integer y = 540
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_semanafactura
integer x = 1865
integer y = 1464
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_semanafactura
boolean visible = false
integer x = 1865
integer y = 1080
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_semanafactura
integer x = 1865
integer y = 900
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_semanafactura
integer x = 82
integer y = 68
integer width = 1541
integer height = 1292
integer taborder = 70
string dataobject = "dw_mues_semanafactura"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Long	ll_Null
String	ls_Columna

ls_Columna = dwo.Name

SetNull(ll_null)

CHOOSE CASE ls_Columna
	CASE "fase_numero"
		IF Duplicado(ls_Columna, data) THEN
			This.SetItem(il_fila, ls_Columna, ll_null)
			RETURN 1
		END IF
		
	Case 'fase_inicio'
		This.SetItem(Row, 'fase_final', RelativeDate(Date(Data), 6))

END CHOOSE

end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;CHOOSE CASE dwo.Name

	CASE "prec_abrevi"
			pb_grabar.Enabled	=	True

END CHOOSE

end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

RETURN 0
end event

