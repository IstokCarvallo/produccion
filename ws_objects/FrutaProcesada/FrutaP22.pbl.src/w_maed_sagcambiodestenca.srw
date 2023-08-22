$PBExportHeader$w_maed_sagcambiodestenca.srw
forward
global type w_maed_sagcambiodestenca from w_mant_encab_deta_csd
end type
type st_11 from statictext within w_maed_sagcambiodestenca
end type
type st_12 from statictext within w_maed_sagcambiodestenca
end type
type em_oficina from editmask within w_maed_sagcambiodestenca
end type
type cbx_archivo from checkbox within w_maed_sagcambiodestenca
end type
type cbx_info from checkbox within w_maed_sagcambiodestenca
end type
type st_5 from statictext within w_maed_sagcambiodestenca
end type
type ddlb_accion from dropdownlistbox within w_maed_sagcambiodestenca
end type
end forward

global type w_maed_sagcambiodestenca from w_mant_encab_deta_csd
integer width = 3310
integer height = 1984
string title = "CAMBIO DE DESTINO DE PALLET"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
st_11 st_11
st_12 st_12
em_oficina em_oficina
cbx_archivo cbx_archivo
cbx_info cbx_info
st_5 st_5
ddlb_accion ddlb_accion
end type
global w_maed_sagcambiodestenca w_maed_sagcambiodestenca

type variables
w_mant_deta_sagcambiodestdeta iw_mantencion

DataWindowChild	dw_planta, dw_cliente

integer		ii_accion
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public subroutine existefolio (string as_columna, string as_valor)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String	ls_oficina, ls_Archivo, ls_Ruta

ls_oficina	=	em_oficina.Text


IF cbx_archivo.Checked THEN
	
	ls_Archivo	=	"\CambioDestino-"+String(Long(istr_mant.argumento[2]))+".xls"

	ids_archivo.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
			
	ids_archivo.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)	
			
	MessageBox("Atención","Archivo Formato Excel, Generado.")
ELSE
	
	str_info	lstr_info
	
	lstr_info.titulo	= "CAMBIO DE DESTINO A PALLET"
	lstr_info.copias	= 1
	
	OpenWithParm(vinf,lstr_info)
	
	//vinf.dw_1.DataObject = "dw_info_sagcambiodestenca"
	
	IF cbx_info.Checked THEN
		vinf.dw_1.DataObject = "dw_info_sagcambiodestenca_pro"
	ELSE	
		vinf.dw_1.DataObject = "dw_info_sagcambiodestenca_pro_nuevo"
	END IF	
	vinf.dw_1.SetTransObject(sqlca)
	
	
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),ii_accion)
		
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("t_oficina.text = '" + ls_oficina + "'")
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
	END IF

END IF

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("plde_codigo",10)
	dw_2.SetTabOrder("sagc_numero",20)
	dw_2.Modify("sagc_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.SetColumn("sagc_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("sagc_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.Modify("sagc_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
END IF
end subroutine

public subroutine habilitaingreso (string columna);Date	ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.sagc_numero[1]) OR dw_2.Object.sagc_numero[1] = 0 OR &
		IsNull(dw_2.Object.sagc_feccam[1]) OR dw_2.Object.sagc_feccam[1] = ld_fecha THEN
		lb_estado = False
	END IF
END IF

pb_grabar.Enabled		=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public subroutine existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.sagc_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "sagc_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dba.Sagcambiodestenca
	WHERE	plde_codigo	=	:li_planta
	AND	sagc_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Sagcambiodestenca")
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	
	This.TriggerEvent("ue_recuperadatos")

	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
ELSE
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	//istr_mant.argumento[3]	= String(li_cliente)
END IF

RETURN
end subroutine

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio 
						[3]	=	Código de Cliente
						[4]	=  Número de Pallet
						
*/
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

ddlb_accion.SelectItem(1)

ii_accion = 1


dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
IF dw_planta.Retrieve(1) = 0 THEN
	dw_planta.insertRow(0)
END IF

dw_2.GetChild("clie_codigo", dw_cliente)
dw_cliente.SetTransObject(sqlca)
IF dw_cliente.Retrieve() = 0 THEN
	dw_cliente.insertRow(0)
END IF

dw_2.SetItem(1, "clie_codigo",gi_codexport)
dw_2.SetItem(1, "plde_codigo",gi_codplanta)
dw_2.Setitem(1, "sagc_feccam",Today())

IF date(now()) > Date('2009-11-01') THEN
	cbx_info.Checked = False
ELSE
	cbx_info.Checked = True
END IF	


istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)

ids_archivo					=	CREATE	DataStore
ids_archivo.DataObject	=	'dw_info_sagcambiodestenca_excel'
ids_archivo.SetTransObject(sqlca)

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

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
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)
	
IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF
	
dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), &
										 Integer(istr_mant.argumento[1]), &
										 Long(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eli_det.Enabled	= True
				pb_ins_det.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eliminar.Enabled	= True
				pb_imprimir.Enabled	= True
								
				IF ll_fila_d > 0 THEN
					pb_imprimir.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
					cbx_archivo.Enabled	= True
					
				ELSE
				pb_ins_det.Enabled	= True
							
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

on w_maed_sagcambiodestenca.create
int iCurrent
call super::create
this.st_11=create st_11
this.st_12=create st_12
this.em_oficina=create em_oficina
this.cbx_archivo=create cbx_archivo
this.cbx_info=create cbx_info
this.st_5=create st_5
this.ddlb_accion=create ddlb_accion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_11
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.em_oficina
this.Control[iCurrent+4]=this.cbx_archivo
this.Control[iCurrent+5]=this.cbx_info
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.ddlb_accion
end on

on w_maed_sagcambiodestenca.destroy
call super::destroy
destroy(this.st_11)
destroy(this.st_12)
destroy(this.em_oficina)
destroy(this.cbx_archivo)
destroy(this.cbx_info)
destroy(this.st_5)
destroy(this.ddlb_accion)
end on

event ue_nuevo;HabilitaEncab(True)


ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	//CASE 0
//		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.TriggerEvent("ue_guardar")
				IF message.DoubleParm = -1 THEN ib_ok = False
			CASE 3
				ib_ok	= False
				RETURN
//		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.Setitem(1, "sagc_feccam",Today())
dw_2.SetColumn("sagc_numero")
dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_sagcambiodestenca, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[5]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= 	False
	istr_mant.borra			= 	False
	istr_mant.Argumento[4]	=	String(dw_1.Object.paen_numero[il_fila])

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event resize;//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_sagcambiodestenca
integer x = 41
integer y = 760
integer width = 2825
integer height = 964
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_sagcambiodestdeta"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_sagcambiodestenca
integer x = 41
integer y = 36
integer width = 2821
integer height = 552
string dataobject = "dw_mant_sagcambiodestenca"
boolean livescroll = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna, ls_null, ls_embq_codigo
Date		ld_nula

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		istr_mant.argumento[3]	= data
		IF F_ValidaCliente(Integer(data)) = False THEN
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			RETURN 1
		END IF
					
	CASE "plde_codigo"
		ExisteFolio(ls_columna, data)
	CASE "sagc_numero"
		ExisteFolio(ls_columna, data)

END CHOOSE

HabilitaIngreso(ls_columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_sagcambiodestenca
integer x = 2976
integer y = 252
end type

event pb_nuevo::clicked;call super::clicked;cbx_archivo.Enabled	= False
					
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_sagcambiodestenca
integer x = 2976
integer y = 480
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_sagcambiodestenca
integer x = 2976
integer y = 696
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_sagcambiodestenca
integer x = 2976
integer y = 928
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_sagcambiodestenca
integer x = 2976
integer y = 1156
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_sagcambiodestenca
integer x = 2976
integer y = 1412
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_sagcambiodestenca
integer x = 2976
integer y = 1592
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_sagcambiodestenca
integer x = 2976
integer y = 72
end type

type st_11 from statictext within w_maed_sagcambiodestenca
integer x = 41
integer y = 584
integer width = 2821
integer height = 156
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_12 from statictext within w_maed_sagcambiodestenca
integer x = 142
integer y = 604
integer width = 640
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Oficina"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_oficina from editmask within w_maed_sagcambiodestenca
integer x = 590
integer y = 612
integer width = 2176
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type cbx_archivo from checkbox within w_maed_sagcambiodestenca
integer x = 50
integer y = 1744
integer width = 320
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Archivo"
end type

type cbx_info from checkbox within w_maed_sagcambiodestenca
integer x = 667
integer y = 1744
integer width = 622
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Formato Antiguo"
end type

type st_5 from statictext within w_maed_sagcambiodestenca
integer x = 1445
integer y = 1744
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Tipo Actividad"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type ddlb_accion from dropdownlistbox within w_maed_sagcambiodestenca
integer x = 1947
integer y = 1732
integer width = 910
integer height = 532
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"Inspección","Tratamiento","Repaletizaje","Interplanta","Otro","",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_accion = index
end event

