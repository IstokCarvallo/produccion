$PBExportHeader$w_mant_mues_ctlcaltiposinspeccion.srw
$PBExportComments$Mantenedor Directo de Tipos de Inspección.
forward
global type w_mant_mues_ctlcaltiposinspeccion from w_mant_directo
end type
end forward

global type w_mant_mues_ctlcaltiposinspeccion from w_mant_directo
integer width = 2373
integer height = 1908
string title = "TIPOS DE INSPECCION"
end type
global w_mant_mues_ctlcaltiposinspeccion w_mant_mues_ctlcaltiposinspeccion

type variables
DataWindowChild	idwc_zona

Integer				ii_tipo, ii_orden
end variables

forward prototypes
public function boolean duplicado (string valor)
end prototypes

public function boolean duplicado (string valor);Long		ll_fila

ll_fila	= dw_1.Find("ccti_codigo = " + valor, + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
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

event open;call super::open;
buscar			= "Código Inspección:ccti_codigo,Nombre Inspección:ccti_descrip"
ordenar			= "Código Inspección:ccti_codigo,Nombre Inspección:ccti_descrip"
end event

on w_mant_mues_ctlcaltiposinspeccion.create
call super::create
end on

on w_mant_mues_ctlcaltiposinspeccion.destroy
call super::destroy
end on

event ue_imprimir;SetPointer(HourGlass!)

Integer	li_zona
Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE TIPOS DE INSPECCION"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcaltipoinspeccion"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

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

event ue_antesguardar;Long		ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]


DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP


For ll_Fila	= 1 To dw_1.RowCount()
		IF Isnull(dw_1.Object.ccti_codigo[ll_Fila]) OR dw_1.Object.ccti_codigo[ll_Fila] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nCódigo Tipo de Inspección"
			ls_colu[li_cont] 	= "ccti_codigo"
		END IF
		
		IF Isnull(dw_1.Object.ccti_descrip[ll_Fila]) OR trim(dw_1.Object.ccti_descrip[ll_Fila]) = "" THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nNombre de Inspección"
			ls_colu[li_cont]	= "ccti_descrip"
		END IF
Next

	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	else
		 Message.DoubleParm = 0
	End If

end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcaltiposinspeccion
boolean visible = false
integer x = 2331
integer y = 132
integer width = 1906
integer height = 244
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcaltiposinspeccion
boolean visible = false
integer x = 2299
integer y = 420
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False
pb_lectura.Enabled	= True
il_fila					= 0

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcaltiposinspeccion
integer x = 2016
integer taborder = 50
end type

event pb_lectura::clicked;call super::clicked;//pb_lectura.Enabled = FALSE
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcaltiposinspeccion
integer x = 2016
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcaltiposinspeccion
integer x = 2016
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcaltiposinspeccion
integer x = 2016
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcaltiposinspeccion
integer x = 2016
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcaltiposinspeccion
integer x = 2016
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcaltiposinspeccion
integer x = 105
integer y = 64
integer width = 1792
integer height = 1664
integer taborder = 70
string dataobject = "dw_mues_ctlcaltipoinspecion"
boolean hscrollbar = true
end type

event dw_1::itemchanged;Long		ll_null 

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "ccti_codigo"
              parent.TriggerEvent("ue_validaregistro")
			IF Duplicado(data) THEN
				This.SetItem(il_fila, "ccti_codigo", ll_null)
				RETURN 1			
			END IF 			

END CHOOSE
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;////IF CurrentRow > 0 AND il_fila > 0 THEN
////	ias_campo[1] = String(dw_1.Object.cctc_codigo[il_fila])
////	ias_campo[2] = dw_1.Object.cctc_nombres[il_fila]
////	ias_campo[3] = dw_1.Object.cctc_abrevi[il_fila]
////END IF
//
//Integer li_Fila
//
//li_Fila = dw_1.RowCount()
//
//IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
//	ib_datos_ok = False
//ELSE
//	il_fila				=	GetRow()
//	pb_grabar.Enabled	=	True
//END IF
//
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;//CHOOSE CASE dwo.Name
//
//	CASE "ccin_abrevi"
//			pb_grabar.Enabled	=	True
//
////	CASE "cctc_codigo"
////			TriggerEvent("ue_validaregistro")
//
//END CHOOSE
//
end event

event dw_1::dwnkey;//parent.TriggerEvent("ue_validaregistro")
//IF Message.DoubleParm = -1 THEN
//    RETURN - 1
//END IF


end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event

