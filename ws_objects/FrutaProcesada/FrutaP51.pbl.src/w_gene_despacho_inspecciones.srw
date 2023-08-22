$PBExportHeader$w_gene_despacho_inspecciones.srw
forward
global type w_gene_despacho_inspecciones from window
end type
type dw_11 from datawindow within w_gene_despacho_inspecciones
end type
type dw_10 from datawindow within w_gene_despacho_inspecciones
end type
type st_4 from statictext within w_gene_despacho_inspecciones
end type
type st_3 from statictext within w_gene_despacho_inspecciones
end type
type sle_mensa from singlelineedit within w_gene_despacho_inspecciones
end type
type em_planilla from editmask within w_gene_despacho_inspecciones
end type
type st_5 from statictext within w_gene_despacho_inspecciones
end type
type st_2 from statictext within w_gene_despacho_inspecciones
end type
type st_1 from statictext within w_gene_despacho_inspecciones
end type
type pb_salir from picturebutton within w_gene_despacho_inspecciones
end type
type pb_grabar from picturebutton within w_gene_despacho_inspecciones
end type
type st_6 from statictext within w_gene_despacho_inspecciones
end type
type dw_1 from datawindow within w_gene_despacho_inspecciones
end type
type dw_2 from datawindow within w_gene_despacho_inspecciones
end type
end forward

global type w_gene_despacho_inspecciones from window
integer width = 2395
integer height = 1052
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_11 dw_11
dw_10 dw_10
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_planilla em_planilla
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
st_6 st_6
dw_1 dw_1
dw_2 dw_2
end type
global w_gene_despacho_inspecciones w_gene_despacho_inspecciones

type variables
str_mant               istr_mant
Date		id_FechaAcceso
Time		it_HoraAcceso

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public function boolean masdeundestino (integer al_planilla)
public function string clienterotulado (integer cliente)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_destinoins, ll_Filas2
Integer		li_PldSag
String		ls_Archivo, ls_Registro, ls_ruta

dw_2.reset()
dw_1.reset()

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)

dw_1.Reset()

ll_Numero	= Long(em_planilla.text)

ll_Filas		= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), ll_Numero)

ll_Filas2	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), ll_Numero)

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")
	sle_mensa.Text = 'Error En Recuperación'
ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_planilla.SetFocus()
	sle_mensa.Text = 'No Existe Información para Generar'
ELSE
	IF dw_1.retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), ll_Numero) < 1 THEN
		Messagebox("Error", "Despacho NO involucra Inspecciones~n~r" + &
									"No se pudo Generar la Inspecpalenc", Exclamation!)
	ELSE
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
		ls_Archivo	= '\Inspecpalenc' + String(ll_Numero, '00000000') + '.CSV'
		IF dw_1.SaveAs(ls_ruta + ls_archivo, CSV!	 ,FALSE) = -1 THEN
			MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especificada~n~r" + &
							ls_ruta + ls_archivo+"~n~r", StopSign!)
			Return
		ELSE
			sle_mensa.Text = 'Archivo Generado en Mis Documentos'
		END IF	
	END IF
	
		IF dw_2.retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), ll_Numero) < 1 THEN
			Messagebox("Error", "Despacho NO involucra Detalle Inspecciones~n~r" + &
										"No se pudo Generar la Inspecpaldet", Exclamation!)
		ELSE
			RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
			ls_Archivo	= '\Inspecpaldet' + String(ll_Numero, '00000000') + '.CSV'
			IF dw_2.SaveAs(ls_ruta + ls_archivo, CSV!	 ,FALSE) = -1 THEN
				MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especificada~n~r" + &
								ls_ruta + ls_archivo+"~n~r", StopSign!)
				Return
			ELSE
				sle_mensa.Text = 'Archivo Generado en Mis Documentos'
			END IF	
		END IF
		
	dw_2.Reset()
	dw_1.Reset()

END IF

em_planilla.SetFocus()
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date		ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	
	SELECT Max(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_numero	=	:al_planilla;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No Existe Despacho. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSEIF IsNull(ld_fecha) THEN
		MessageBox("Atención", "No existe Despacho. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSE
		sle_mensa.text		= ""
		pb_grabar.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function boolean masdeundestino (integer al_planilla);Integer	li_codexp, li_planta, li_variosdestinos

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	
	SELECT Count(distinct dest_codigo)
		INTO	:li_variosdestinos
		FROM	dbo.DESPAFRIGOEN as DE, dbo.PALLETENCAB as PE, dbo.DESPAFRIGODE as DD
		WHERE	de.plde_codigo =	:li_planta
		AND	de.clie_codigo	=	:li_codexp
		AND	de.defe_plasag	=	:al_planilla
		AND   de.clie_codigo	=	dd.clie_codigo
		AND   de.plde_codigo	=	dd.plde_codigo
		AND   de.defe_numero =  dd.defe_numero
		AND   pe.clie_codigo	=	dd.clie_codigo
		AND   pe.plde_codigo	=	dd.plde_codigo
		AND   pe.paen_numero =  dd.paen_numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSEIF li_variosdestinos > 1 THEN
		MessageBox("Atención", "Planilla S.A.G. Incluye Mas de un Destino.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	 ELSE
		pb_grabar.Enabled	= True
		RETURN True
	 END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function string clienterotulado (integer cliente);String	ls_rotulacion

SELECT clie_rotula
INTO   :ls_rotulacion
FROM dbo.clientesprod
WHERE clie_codigo = :Cliente;

IF IsNull(ls_rotulacion) THEN ls_rotulacion	=	String(Cliente)

RETURN ls_rotulacion
end function

on w_gene_despacho_inspecciones.create
this.dw_11=create dw_11
this.dw_10=create dw_10
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_planilla=create em_planilla
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_6=create st_6
this.dw_1=create dw_1
this.dw_2=create dw_2
this.Control[]={this.dw_11,&
this.dw_10,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_planilla,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.st_6,&
this.dw_1,&
this.dw_2}
end on

on w_gene_despacho_inspecciones.destroy
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_planilla)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.dw_2)
end on

event open;x	=	0
y	=	0
This.Icon	=	Gstr_apl.Icono

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)
dw_10.SetItem(1,"clie_codigo",gi_codexport)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

istr_mant.argumento[1]	=	String(gi_codexport,'000')
istr_mant.argumento[2]	=	String(gi_codplanta)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							


end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_11 from datawindow within w_gene_despacho_inspecciones
integer x = 576
integer y = 388
integer width = 969
integer height = 100
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	String(data)
end event

type dw_10 from datawindow within w_gene_despacho_inspecciones
integer x = 576
integer y = 256
integer width = 1157
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(Integer(data),'000')
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type st_4 from statictext within w_gene_despacho_inspecciones
integer x = 133
integer y = 396
integer width = 402
integer height = 64
integer taborder = 40
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

type st_3 from statictext within w_gene_despacho_inspecciones
integer x = 133
integer y = 264
integer width = 402
integer height = 64
integer taborder = 20
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

type sle_mensa from singlelineedit within w_gene_despacho_inspecciones
integer x = 178
integer y = 708
integer width = 1632
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_planilla from editmask within w_gene_despacho_inspecciones
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 576
integer y = 524
integer width = 443
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;IF Long(This.Text) <> 0 THEN
	IF ExistePlanilla(Long(This.Text)) = False THEN
		em_planilla.Text	=	''
		em_planilla.SetFocus()
	ELSE
		sle_mensa.Text = 'Listo Para Generar Archivo'
	END IF
END IF	
end event

type st_5 from statictext within w_gene_despacho_inspecciones
integer x = 78
integer y = 68
integer width = 1815
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Plano Despacho (Inspecciones)"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_despacho_inspecciones
integer x = 133
integer y = 528
integer width = 448
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nº Despacho"
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_despacho_inspecciones
integer x = 82
integer y = 224
integer width = 1815
integer height = 416
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_despacho_inspecciones
integer x = 1989
integer y = 620
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_despacho_inspecciones
integer x = 1989
integer y = 360
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type st_6 from statictext within w_gene_despacho_inspecciones
integer x = 82
integer y = 640
integer width = 1815
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_gene_despacho_inspecciones
boolean visible = false
integer x = 1966
integer y = 76
integer width = 155
integer height = 120
boolean bringtotop = true
string dataobject = "dw_despacho_inspecciones_enc"
boolean livescroll = true
end type

type dw_2 from datawindow within w_gene_despacho_inspecciones
boolean visible = false
integer x = 1966
integer y = 212
integer width = 155
integer height = 108
integer taborder = 90
boolean bringtotop = true
string dataobject = "dw_despacho_inspecciones_det"
boolean maxbox = true
boolean livescroll = true
end type

event clicked;//This.Print()
end event

