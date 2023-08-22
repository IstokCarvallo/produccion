$PBExportHeader$w_mant_mues_comunas.srw
forward
global type w_mant_mues_comunas from w_mant_tabla
end type
end forward

global type w_mant_mues_comunas from w_mant_tabla
integer width = 2885
string title = "COMUNAS"
end type
global w_mant_mues_comunas w_mant_mues_comunas

type variables
w_mant_deta_comunasexp iw_mantencion
DataWindowChild	idwc_ciudad
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE COMUNAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_comunas"

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
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(0)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_comunas.create
call super::create
end on

on w_mant_mues_comunas.destroy
call super::destroy
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
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
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event open;call super::open;dw_1.GetChild("ciud_codigo", idwc_ciudad)
idwc_ciudad.SetTransObject(sqlca)
IF idwc_ciudad.Retrieve(0)	=	0 THEN
	MessageBox("Atención","No Existe Registro de Ciudades",Exclamation!)
	idwc_ciudad.InsertRow(0)
END IF	
buscar	= "Código:Ncomu_codigo,Descripción:Scomu_nombre"
ordenar	= "Código:comu_codigo,Descripción:comu_nombre"
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_comunas
integer y = 64
integer width = 2336
integer height = 1188
string dataobject = "dw_mues_comunas"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_comunas
boolean visible = false
integer x = 370
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_comunas
string tag = "Selección de Parámetros"
integer x = 2523
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_comunas
string tag = ""
boolean visible = false
integer x = 2523
integer y = 400
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_comunas
string tag = ""
integer x = 2523
integer y = 592
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_comunas
string tag = ""
integer x = 2523
integer y = 768
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_comunas
string tag = ""
integer x = 2523
integer y = 972
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_comunas
string tag = ""
integer x = 2523
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_comunas
string tag = ""
integer x = 2523
integer y = 1524
end type

