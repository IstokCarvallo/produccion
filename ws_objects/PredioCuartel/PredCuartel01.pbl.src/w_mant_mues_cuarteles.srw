$PBExportHeader$w_mant_mues_cuarteles.srw
forward
global type w_mant_mues_cuarteles from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_cuarteles
end type
end forward

global type w_mant_mues_cuarteles from w_mant_tabla
integer width = 3579
string title = "CUARTELES"
st_1 st_1
end type
global w_mant_mues_cuarteles w_mant_mues_cuarteles

type variables
w_mant_deta_cuarteles iw_mantencion

DataWindowChild  idwc_especie, idwc_variedad, idwc_riego, idwc_conduccion
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

lstr_info.titulo	= "MAESTRO DE CUARTELES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_cuarteles"

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

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve()
	
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

on w_mant_mues_cuarteles.create
int iCurrent
call super::create
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
end on

on w_mant_mues_cuarteles.destroy
call super::destroy
destroy(this.st_1)
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

event open;call super::open;buscar	= "Código Cuartel:prcc_codigo,Nombre:prcc_nombre"
ordenar	= "Código Cuartel:prcc_codigo,Nombre:prcc_nombre"

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()

dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(99)
idwc_variedad.SetSort("vari_nombre A")
idwc_variedad.Sort()
//idwc_variedad.InsertRow(0)

dw_1.GetChild("siri_codigo", idwc_riego)
idwc_riego.SetTransObject(sqlca)
idwc_riego.Retrieve()

dw_1.GetChild("sico_codigo", idwc_conduccion)
idwc_conduccion.SetTransObject(sqlca)
idwc_conduccion.Retrieve()





end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_cuarteles
integer x = 59
integer y = 72
integer width = 3090
integer height = 1680
integer taborder = 10
string dataobject = "dw_mues_cuarteles"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_cuarteles
boolean visible = false
integer x = 0
integer y = 748
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_cuarteles
integer x = 3273
integer y = 132
integer taborder = 20
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_cuarteles
integer x = 3273
integer y = 460
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;//pb_insertar.Enabled	= False
//pb_imprimir.Enabled	= False
//pb_eliminar.Enabled	= False
//pb_grabar.Enabled		= False
//
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_cuarteles
integer x = 3273
integer y = 636
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_cuarteles
integer x = 3273
integer y = 804
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_cuarteles
integer x = 3273
integer y = 972
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_cuarteles
integer x = 3273
integer y = 1140
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_cuarteles
integer x = 3273
integer y = 1512
end type

type st_1 from statictext within w_mant_mues_cuarteles
boolean visible = false
integer x = 73
integer y = 76
integer width = 2368
integer height = 212
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

