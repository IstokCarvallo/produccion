$PBExportHeader$w_mant_mues_prodcalidadmercado.srw
forward
global type w_mant_mues_prodcalidadmercado from w_mant_directo
end type
end forward

global type w_mant_mues_prodcalidadmercado from w_mant_directo
integer width = 2651
string title = "CALIDAD POR MERCADO (CONDICION)"
end type
global w_mant_mues_prodcalidadmercado w_mant_mues_prodcalidadmercado

type variables
datawindowchild        dw_mues_plantadesp

integer    ii_tipo, ii_especie

end variables

forward prototypes
public function boolean duplicado (string codigo)
end prototypes

public function boolean duplicado (string codigo);Long		ll_fila
String	ls_codigo
	
ll_fila	= dw_1.Find("calm_codigo = " + codigo , 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Calidad Mercado, ya fue ingresado anteriormente",Information!, Ok!)
	Codigo = ""
	RETURN True
ELSE
	RETURN False
END IF

		
end function

on w_mant_mues_prodcalidadmercado.create
call super::create
end on

on w_mant_mues_prodcalidadmercado.destroy
call super::destroy
end on

event open;call super::open;buscar			=	"Código:Ncalm_codigo,Nombre:Scalm_nombre"
ordenar			=	"Código:calm_codigo,Nombre:calm_nombre"
is_ultimacol		=	'calm_abrevi'
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

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
		ias_campo[1]			= String(dw_1.Object.calm_codigo[1])
		
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
		ias_campo[1]			= ""
		ias_campo[2]			= ""
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;istr_mant.argumento[1]	=	""
dw_1.SetColumn("calm_codigo")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String	ls_argumento, ls_argum1
str_info	lstr_info

lstr_info.titulo	= "CALIDAD MERCADO"
lstr_info.copias	= 1
OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_prodcalidadmercado"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	IF Not gs_Ambiente = 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, lstr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.calm_codigo[il_fila]) OR dw_1.Object.calm_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Calidad"
	ls_colu[li_cont]	= "calm_codigo"
END IF

IF Isnull(dw_1.Object.calm_nombre[il_fila]) OR dw_1.Object.calm_nombre[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre de Calidad"
	ls_colu[li_cont]	= "calm_nombre"
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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_prodcalidadmercado
boolean visible = false
integer x = 96
integer y = 60
integer width = 2030
integer height = 228
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_prodcalidadmercado
integer x = 2254
integer y = 364
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_prodcalidadmercado
integer x = 2245
integer y = 128
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_prodcalidadmercado
integer x = 2245
integer y = 720
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_prodcalidadmercado
integer x = 2245
integer y = 540
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_prodcalidadmercado
integer x = 2245
integer y = 1396
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_prodcalidadmercado
integer x = 2245
integer y = 1080
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_prodcalidadmercado
integer x = 2245
integer y = 900
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_prodcalidadmercado
integer y = 48
integer width = 2085
integer height = 1768
string dataobject = "dw_mant_mues_prodcalidadmercado"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_null, ls_Columna

SetNull(ls_null)
ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "calm_codigo"		
		IF Duplicado(data) THEN
			This.SetItem(Row, ls_Columna,Long(ls_null))
			RETURN 1
		END IF
		
END CHOOSE
end event

event dw_1::dwnkey;//
end event

