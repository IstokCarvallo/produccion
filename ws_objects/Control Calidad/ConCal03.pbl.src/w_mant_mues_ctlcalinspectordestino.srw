$PBExportHeader$w_mant_mues_ctlcalinspectordestino.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_mues_ctlcalinspectordestino from w_mant_directo
end type
end forward

global type w_mant_mues_ctlcalinspectordestino from w_mant_directo
integer width = 2981
string title = "MAESTRO DE INSPECTORES POR DESTINO"
end type
global w_mant_mues_ctlcalinspectordestino w_mant_mues_ctlcalinspectordestino

type variables
Integer				ii_tipo, ii_orden
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	dw_1.Object.insp_codigo[il_fila]

CHOOSE CASE columna
	CASE "insp_codigo"
		li_codigo	=	Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("insp_codigo = " + String(li_codigo), + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
//		dw_1.SetRow(1)
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

event open;call super::open;buscar			= "Código Inspector:Ninsp_codigo,Nombre Inspector:Sinsp_nombre,Abreviación:Sinsp_abrevi"
ordenar			= "Código Inspector:insp_codigo,Nombre Inspector:insp_nombre,Abreviación:insp_abrevi"

end event

on w_mant_mues_ctlcalinspectordestino.create
call super::create
end on

on w_mant_mues_ctlcalinspectordestino.destroy
call super::destroy
end on

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.insp_codigo[il_fila]) OR dw_1.Object.insp_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Inspector"
	ls_colu[li_cont]	= "insp_codigo"
END IF

IF Isnull(dw_1.Object.insp_nombre[il_fila]) OR dw_1.Object.insp_nombre[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre del Inspector"
	ls_colu[li_cont]	= "insp_nombre"
END IF

IF Isnull(dw_1.Object.insp_abrevi[il_fila]) OR dw_1.Object.insp_abrevi[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nAbreviación"
	ls_colu[li_cont]	= "insp_abrevi"
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

lstr_info.titulo	= "MAESTRO DE INSPECTORES POR DESTINO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalinspectordestino"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF dw_1.RowCount() = 0 THEN
	pb_Grabar.Enabled		=	False
	pb_Eliminar.Enabled	=	False
	pb_Imprimir.Enabled	=	False
ELSE
	IF Isnull(dw_1.Object.insp_codigo[il_fila]) OR dw_1.Object.insp_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Inspector"
		ls_colu[li_cont]	= "insp_codigo"
	END IF
	
	IF Isnull(dw_1.Object.insp_nombre[il_fila]) OR trim(dw_1.Object.insp_nombre[il_fila]) = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nNombre del Inspector"
		ls_colu[li_cont]	= "insp_nombre"
	END IF
	
	IF Isnull(dw_1.Object.insp_abrevi[il_fila]) OR trim(dw_1.Object.insp_abrevi[il_fila]) = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nAbreviación"
		ls_colu[li_cont]	= "insp_abrevi"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF	
END IF




end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalinspectordestino
boolean visible = false
integer x = 1221
integer y = 1584
integer width = 256
integer height = 132
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 408
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 136
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 792
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 612
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 1536
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 1152
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalinspectordestino
integer x = 2597
integer y = 972
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalinspectordestino
integer x = 82
integer y = 68
integer width = 2336
integer height = 1292
integer taborder = 70
string dataobject = "dw_mues_ctlcalinspectordestino"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_null

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "insp_codigo"
		IF Duplicado(dwo.Name, data) THEN
			This.SetItem(il_fila, "insp_codigo", ll_null)
			RETURN 1
		END IF

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
	CASE "dade_abrevi"
			pb_grabar.Enabled	=	True

END CHOOSE
end event

