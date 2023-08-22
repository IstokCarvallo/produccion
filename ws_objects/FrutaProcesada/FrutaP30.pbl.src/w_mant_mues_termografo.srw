$PBExportHeader$w_mant_mues_termografo.srw
forward
global type w_mant_mues_termografo from w_mant_tabla
end type
type st_4 from statictext within w_mant_mues_termografo
end type
type dw_11 from datawindow within w_mant_mues_termografo
end type
type st_1 from statictext within w_mant_mues_termografo
end type
type dw_2 from datawindow within w_mant_mues_termografo
end type
end forward

global type w_mant_mues_termografo from w_mant_tabla
integer width = 3611
integer height = 1884
string title = "MAESTRO DE TERMOGRAFOS"
st_4 st_4
dw_11 dw_11
st_1 st_1
dw_2 dw_2
end type
global w_mant_mues_termografo w_mant_mues_termografo

type variables
w_mant_deta_termografo iw_mantencion

DataWindowChild	idwc_cliente, idwc_planta

uo_cliente		iuo_cliente
uo_plantadesp	iuo_planta

end variables

forward prototypes
protected function boolean wf_actualiza_db ()
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
		pb_lectura.Enabled = False
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

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

lstr_info.titulo	= "MAESTRO TERMOGRAFOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_termografos"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]))

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
dw_1.reset()

DO
	ll_fila	= dw_1.Retrieve(integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]))
	
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

on w_mant_mues_termografo.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_11=create dw_11
this.st_1=create st_1
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_11
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.dw_2
end on

on w_mant_mues_termografo.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_11)
destroy(this.st_1)
destroy(this.dw_2)
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

event open;call super::open;
dw_11.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
dw_11.InsertRow(0)
dw_11.SetItem(1,"clie_codigo",gi_CodExport)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"plde_codigo",gi_CodPlanta)

iuo_planta		=	CREATE uo_plantadesp
iuo_cliente		=	CREATE uo_cliente


istr_mant.argumento[2] = String(gi_CodExport)
istr_mant.argumento[1] = String(gi_CodPlanta)
end event

event ue_modifica;call super::ue_modifica;//istr_mant.argumento[2]	=	dw_11.Object.espe_codigo[1]

IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_termografo
integer x = 37
integer y = 436
integer width = 3122
integer height = 1132
integer taborder = 20
string dataobject = "dw_mues_termografo"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_termografo
integer x = 37
integer width = 3122
integer height = 324
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_termografo
integer x = 3250
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_termografo
integer x = 3223
integer y = 296
integer taborder = 0
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled = true

dw_11.Reset()
idwc_cliente.Retrieve()
dw_11.InsertRow(0)
dw_11.SetItem(1,"clie_codigo",gi_CodExport)

dw_2.Reset()
idwc_planta.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"plde_codigo",gi_CodPlanta)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_termografo
integer x = 3250
integer y = 372
integer taborder = 30
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_termografo
integer x = 3250
integer y = 588
integer taborder = 40
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_termografo
integer x = 3250
integer y = 804
integer taborder = 50
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_termografo
integer x = 3250
integer y = 1020
integer taborder = 60
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_termografo
integer x = 3250
integer y = 1516
integer taborder = 70
end type

type st_4 from statictext within w_mant_mues_termografo
integer x = 178
integer y = 196
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_11 from datawindow within w_mant_mues_termografo
integer x = 421
integer y = 172
integer width = 1198
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF iuo_cliente.Existe(integer(data), True, Sqlca) THEN
	istr_mant.argumento[2]	=	data
	pb_lectura.Enabled 		= True
ELSE
	dw_11.SetItem(1, "clie_codigo", Integer(li_null))
	pb_lectura.Enabled = False
	Return 0
END IF	


end event

type st_1 from statictext within w_mant_mues_termografo
integer x = 1691
integer y = 196
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_mues_termografo
integer x = 1925
integer y = 172
integer width = 1184
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF iuo_planta.Existe(integer(data), True, Sqlca) THEN
	istr_mant.argumento[1]	=	data
	pb_lectura.Enabled 		= True
ELSE
	dw_2.SetItem(1, "plde_codigo", Integer(li_null))
	pb_lectura.Enabled = False
	Return 0
END IF	


end event

