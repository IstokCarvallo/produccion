$PBExportHeader$w_mant_mues_spro_tarifafrutacomercial.srw
forward
global type w_mant_mues_spro_tarifafrutacomercial from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_spro_tarifafrutacomercial
end type
type st_2 from statictext within w_mant_mues_spro_tarifafrutacomercial
end type
type fechaini from editmask within w_mant_mues_spro_tarifafrutacomercial
end type
type dw_2 from datawindow within w_mant_mues_spro_tarifafrutacomercial
end type
type fechafin from editmask within w_mant_mues_spro_tarifafrutacomercial
end type
type st_3 from statictext within w_mant_mues_spro_tarifafrutacomercial
end type
end forward

global type w_mant_mues_spro_tarifafrutacomercial from w_mant_tabla
integer width = 3086
string title = "TARIFA FRUTA COMERCIAL"
event ue_nuevovalorfcomercial ( )
st_1 st_1
st_2 st_2
fechaini fechaini
dw_2 dw_2
fechafin fechafin
st_3 st_3
end type
global w_mant_mues_spro_tarifafrutacomercial w_mant_mues_spro_tarifafrutacomercial

type variables
w_mant_deta_spro_tarifafrutacomercial	iw_mantencion

Date		id_FechaInicio, id_FechaTermino
DateTime	id_FechaSistema

DataWindowChild	idwc_fechas
end variables

on w_mant_mues_spro_tarifafrutacomercial.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.fechaini=create fechaini
this.dw_2=create dw_2
this.fechafin=create fechafin
this.st_3=create st_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.fechaini
this.Control[iCurrent+4]=this.dw_2
this.Control[iCurrent+5]=this.fechafin
this.Control[iCurrent+6]=this.st_3
end on

on w_mant_mues_spro_tarifafrutacomercial.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.fechaini)
destroy(this.dw_2)
destroy(this.fechafin)
destroy(this.st_3)
end on

event open;call super::open;
FechaIni.Text = String (Today(), "dd/mm/yyyy")
FechaIni.setfocus()

dw_2.GetChild("tafc_fecham",idwc_fechas)
idwc_Fechas.SetTransObject(SQLCA)
idwc_Fechas.Retrieve()

dw_2.SetTransObject(SQLCA)
dw_2.InsertRow(0)

SELECT	pate_inicio, pate_termin
	INTO  :id_FechaInicio, :id_FechaTermino
	FROM	dbo.paramtemporada
	WHERE	pate_vigent = 1;
	
id_FechaSistema	=	F_FechaHora()
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_Fila, ll_Respuesta

DO
	ll_Fila	= dw_1.Retrieve(Date(istr_mant.argumento[1]), -1)
	
	IF ll_Fila = -1 THEN
		ll_Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_Fila > 0 THEN
		
		dw_1.SetRow(1)
		dw_1.SelectRow(1, True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE ll_Respuesta = 1

IF ll_Respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_borrar();IF dw_1.rowcount() < 1 THEN RETURN

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

event ue_antesguardar;Long 		ll_Fila, ll_Filas, ll_FilaInsert
Integer	li_Secuencia
Date		ld_FechaInicio, ld_TerminoAnt

ld_FechaInicio	=	Date(istr_Mant.Argumento[1])
ld_TerminoAnt	=	RelativeDate(Date(ld_FechaInicio), -1)

ll_Filas			=	dw_1.RowCount()

dw_1.SetRedraw(False)

SELECT Max(Isnull(tafc_secuen,0))
	INTO	:li_Secuencia
	FROM	dbo.spro_tarifafrutacomercial
	WHERE	tafc_fecham = :ld_FechaInicio;

IF IsNull(li_Secuencia) THEN li_Secuencia = 1

FOR ll_fila = 1 to ll_Filas
	
	IF dw_1.GetItemStatus(ll_Fila, 0, primary!) = NewModified! THEN
		
		li_Secuencia	++
		
		dw_1.Object.tafc_fecham[ll_Fila]	=	ld_FechaInicio
		dw_1.Object.tafc_fechat[ll_Fila]	=	id_FechaTermino
		dw_1.Object.tafc_secuen[ll_Fila]	=	li_Secuencia
		
	ELSEIF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = DataModified! THEN
		
		IF dw_1.Object.tafc_fecham[ll_Fila] < ld_FechaInicio THEN
			
			ll_FilaInsert	=	dw_1.RowCount() + 1
			
			dw_1.RowsCopy(ll_Fila,ll_Fila,Primary!,dw_1,ll_FilaInsert,Primary!)
	
			li_Secuencia	++
			
			dw_1.Object.tafc_fechat[ll_Fila]			=	ld_TerminoAnt
			dw_1.Object.tafc_preuni[ll_Fila]			=	dw_1.Object.tafc_preuni.Primary.Original[ll_Fila]
			dw_1.Object.tafc_fecham[ll_FilaInsert]	=	ld_FechaInicio
			dw_1.Object.tafc_secuen[ll_FilaInsert]	=	li_Secuencia
			dw_1.Object.tafc_fechat[ll_FilaInsert]	=	id_FechaTermino
			
		END IF
	END IF
	
NEXT

dw_1.SetRedraw(True)

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "MAESTRO TARIFA FRUTA GRANEL"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_spro_tarifafrutacomercial"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Date(istr_mant.argumento[1]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_modifica();call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_guardar();call super::ue_guardar;IF Message.DoubleParm = -1 THEN RETURN

This.TriggerEvent("ue_recuperadatos")
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_spro_tarifafrutacomercial
integer x = 73
integer y = 340
integer width = 2533
integer height = 1476
integer taborder = 0
string dataobject = "dw_mues_spro_tarifafrutacomercial"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_spro_tarifafrutacomercial
integer x = 73
integer y = 52
integer width = 2533
integer height = 272
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 240
integer taborder = 30
end type

event pb_lectura::clicked;IF IsDate(fechaini.Text) THEN
	fechaini.Enabled	=	False
	dw_2.Enabled		=	False
ELSE
	MessageBox("Atención","Debe seleccionar una Fecha para las Tarifas")
	RETURN
END IF

CALL SUPER::Clicked
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 536
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;Integer li_null
SetNull(li_null)

idwc_Fechas.SetTransObject(SQLCA)
idwc_Fechas.Retrieve()

fechaini.Enabled	=	True
dw_2.Enabled		=	True
dw_2.SetItem(1,"tafc_fecham",Date(li_null))
fechaini.SetFocus()

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 716
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 896
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 1076
integer taborder = 50
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 1256
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_spro_tarifafrutacomercial
integer x = 2683
integer y = 1640
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_spro_tarifafrutacomercial
integer x = 338
integer y = 164
integer width = 261
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Inicio"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_spro_tarifafrutacomercial
integer x = 1737
integer y = 164
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Otras"
boolean focusrectangle = false
end type

type fechaini from editmask within w_mant_mues_spro_tarifafrutacomercial
integer x = 517
integer y = 152
integer width = 402
integer height = 88
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;IF IsDate(This.Text) THEN
	IF Date(This.Text) >= Date(id_FechaInicio) AND Date(This.Text) <= Date(id_FechaTermino) THEN
		istr_mant.argumento[1]	=	This.Text
		istr_Mant.Argumento[2]	=	String(Date(id_FechaTermino),'dd/mm/yyyy')
	ELSE
		MessageBox("Atención","Fecha de Inicio debe estar en el Rango de Temporada Vigente")
		This.Text	=	""
		RETURN
	END IF
END IF
end event

type dw_2 from datawindow within w_mant_mues_spro_tarifafrutacomercial
integer x = 1947
integer y = 152
integer width = 480
integer height = 88
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_fechainitarifa"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
IF Row > 0 THEN
	istr_Mant.Argumento[1]	=	Mid(Data,1,10)
	FechaIni.Text				=	Mid(Data,1,10)
END IF



end event

type fechafin from editmask within w_mant_mues_spro_tarifafrutacomercial
boolean visible = false
integer x = 1239
integer y = 152
integer width = 402
integer height = 88
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;IF IsDate(This.Text) THEN
	IF Date(This.Text) >= Date(id_FechaInicio) AND Date(This.Text) <= Date(id_FechaTermino) THEN
		istr_mant.argumento[1]	=	This.Text
		istr_Mant.Argumento[2]	=	String(Date(id_FechaTermino),'dd/mm/yyyy')
	ELSE
		MessageBox("Atención","Fecha de Inicio debe estar en el Rango de Temporada Vigente")
		This.Text	=	""
		RETURN
	END IF
END IF
end event

type st_3 from statictext within w_mant_mues_spro_tarifafrutacomercial
boolean visible = false
integer x = 983
integer y = 164
integer width = 261
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Termino"
boolean focusrectangle = false
end type

