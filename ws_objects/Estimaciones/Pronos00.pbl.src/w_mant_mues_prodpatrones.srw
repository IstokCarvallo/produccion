$PBExportHeader$w_mant_mues_prodpatrones.srw
forward
global type w_mant_mues_prodpatrones from w_mant_directo
end type
end forward

global type w_mant_mues_prodpatrones from w_mant_directo
integer width = 2802
integer height = 1812
string title = "PATRONES"
end type
global w_mant_mues_prodpatrones w_mant_mues_prodpatrones

type variables
datawindowchild        dw_mues_plantadesp

integer    ii_tipo, ii_especie

end variables

forward prototypes
public function boolean duplicado (string codigo)
end prototypes

public function boolean duplicado (string codigo);Long		ll_fila
String	ls_codigo

	
	ll_fila	= dw_1.Find("patr_codigo = " + codigo , 1, dw_1.RowCount())
	
	
	IF ll_fila > 0 and ll_fila <> il_fila THEN
		MessageBox("Error","Código de Patrones, ya fue ingresado anteriormente",Information!, Ok!)
		Codigo = ""
		RETURN True
	ELSE
		RETURN False
	END IF

		
end function

on w_mant_mues_prodpatrones.create
call super::create
end on

on w_mant_mues_prodpatrones.destroy
call super::destroy
end on

event open;call super::open;buscar			=	"Código:Npatr_codigo,Nombre:Npatr_nombre"
ordenar			=	"Código:patr_codigo,Nombre:patr_nombre"
is_ultimacol		=	'patr_abrevi'
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True
		il_fila					= 1
		ias_campo[1]			= String(dw_1.Object.patr_codigo[1])
		
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
		ias_campo[1]			= ""
		ias_campo[2]			= ""
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo();call super::ue_nuevo;istr_mant.argumento[1]	=	""
dw_1.SetColumn("patr_codigo")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String	ls_argumento, ls_argum1
str_info	lstr_info

lstr_info.titulo	= "PATRONES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_prodpatrones"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_validaregistro();Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.patr_codigo[il_fila]) OR dw_1.Object.patr_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Patrones"
	ls_colu[li_cont]	= "patr_codigo"
END IF

IF Isnull(dw_1.Object.patr_nombre[il_fila]) OR dw_1.Object.patr_nombre[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre de Patrón"
	ls_colu[li_cont]	= "patr_nombre"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return
IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar();call super::ue_antesguardar;IF il_fila > 0 THEN
	//Chequeo de última fila registrada en caso de mantención.
	TriggerEvent("ue_validaregistro")
END IF
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
		il_fila = 0
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_prodpatrones
boolean visible = false
integer x = 347
integer y = 1672
integer width = 603
integer height = 160
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_prodpatrones
integer x = 2446
integer y = 376
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_prodpatrones
integer x = 2437
integer y = 140
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_prodpatrones
integer x = 2437
integer y = 732
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_prodpatrones
integer x = 2437
integer y = 552
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_prodpatrones
integer x = 2437
integer y = 1408
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_prodpatrones
integer x = 2437
integer y = 1092
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_prodpatrones
integer x = 2437
integer y = 912
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_prodpatrones
integer x = 91
integer y = 40
integer width = 2121
integer height = 1376
string dataobject = "dw_mant_mues_prodpatrones"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_null

SetNull(ls_null)
CHOOSE CASE GetColumnName()
		
	CASE "patr_codigo"		
		IF Duplicado(data) THEN
			This.SetItem(il_fila, "patr_codigo",Long(ls_null))
			RETURN 1
		END IF
		
END CHOOSE
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;//IF CurrentRow > 0 AND il_fila > 0 THEN
//	ias_campo[1] = String(dw_1.Object.mate_codigo[il_fila])
//	ias_campo[2] = String(dw_1.Object.vama_valuni[il_fila])
//END IF


end event

