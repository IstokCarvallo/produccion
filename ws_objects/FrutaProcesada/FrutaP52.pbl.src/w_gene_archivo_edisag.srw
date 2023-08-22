$PBExportHeader$w_gene_archivo_edisag.srw
forward
global type w_gene_archivo_edisag from window
end type
type dw_2 from datawindow within w_gene_archivo_edisag
end type
type st_7 from statictext within w_gene_archivo_edisag
end type
type dw_1 from datawindow within w_gene_archivo_edisag
end type
type em_fecha from editmask within w_gene_archivo_edisag
end type
type dw_11 from datawindow within w_gene_archivo_edisag
end type
type dw_10 from datawindow within w_gene_archivo_edisag
end type
type st_4 from statictext within w_gene_archivo_edisag
end type
type st_3 from statictext within w_gene_archivo_edisag
end type
type em_planilla from editmask within w_gene_archivo_edisag
end type
type st_5 from statictext within w_gene_archivo_edisag
end type
type st_2 from statictext within w_gene_archivo_edisag
end type
type st_1 from statictext within w_gene_archivo_edisag
end type
type pb_salir from picturebutton within w_gene_archivo_edisag
end type
type pb_grabar from picturebutton within w_gene_archivo_edisag
end type
type gb_2 from groupbox within w_gene_archivo_edisag
end type
type gb_1 from groupbox within w_gene_archivo_edisag
end type
type sle_mensa from singlelineedit within w_gene_archivo_edisag
end type
type st_6 from statictext within w_gene_archivo_edisag
end type
end forward

global type w_gene_archivo_edisag from window
integer width = 2281
integer height = 1248
boolean titlebar = true
string title = "GENERA ARCHIVO"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 30586022
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
dw_2 dw_2
st_7 st_7
dw_1 dw_1
em_fecha em_fecha
dw_11 dw_11
dw_10 dw_10
st_4 st_4
st_3 st_3
em_planilla em_planilla
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
sle_mensa sle_mensa
st_6 st_6
end type
global w_gene_archivo_edisag w_gene_archivo_edisag

type variables
str_mant               istr_mant
Date		id_FechaAcceso
Time		it_HoraAcceso

DataWindowChild	idwc_cliente, idwc_planta, idwc_tiposag
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public function string buscaregionplanta (integer planta)
public function integer buscaplantasag (integer planta)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, li_PldSag, ll_Numero, ll_FilasPallets
Integer		li_camiones
String		ls_Archivo, ls_Registro, ls_region

dw_1.reset()
dw_1.SetTransObject(Sqlca)
dw_1.Reset()

sle_mensa.text	= "Recopilando Información"

ll_Numero	= 	Long(em_planilla.text)

ls_region	=	Buscaregionplanta(Integer(istr_mant.argumento[2]))
li_PldSag	=	Buscaplantasag(Integer(istr_mant.argumento[2]))

ll_Filas		= 	dw_1.Retrieve(Dec(istr_mant.argumento[1]), dec(istr_mant.argumento[2]), &
				Dec(ll_Numero),dec(istr_mant.argumento[6]))

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")
	Return
ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_planilla.SetFocus()
	Return
END IF
	
ls_Archivo	= ls_region + String(li_PldSag,'000')+String(ll_Numero,'0000000') + ".TXT"

IF dw_1.SaveAs(gs_disco+":\GeneradosSAAM\" + ls_Archivo, Text!, False) = -1 THEN
	MessageBox("Atención","No se pudo generar el archivo "+ls_Archivo)
	sle_mensa.text	= "Archivo " + ls_Archivo + " No generado"
	Return
ELSE
	sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
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
	
	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dba.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.Text = ''
		em_planilla.SetFocus()
		RETURN False
	ELSEIF IsNull(ld_fecha) THEN
					MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
									Exclamation!, Ok!)
					pb_grabar.Enabled	= False
					em_planilla.Text = ''
					em_planilla.SetFocus()
					RETURN False
		 ELSE
					em_fecha.text		= String(ld_fecha)
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

public function string buscaregionplanta (integer planta);Integer	li_region
String	ls_region

ls_region	=	'0'

SELECT plde_region
INTO	:li_region
FROM dba.PLANTADESP
WHERE	plde_codigo=:Planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantadesp")
	
	RETURN ls_region
	
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN ls_region
END IF	

ls_region	=	String(li_region)

IF li_region = 13 THEN
	RETURN "M"
ELSE
	RETURN ls_region
END IF

end function

public function integer buscaplantasag (integer planta);long	li_codmul

SELECT plde_codmul
INTO	:li_codmul
FROM dba.PLANTADESP
WHERE	plde_codigo=:Planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantadesp")
	
	RETURN li_codmul
	
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN li_codmul
END IF	

RETURN li_codmul

end function

on w_gene_archivo_edisag.create
this.dw_2=create dw_2
this.st_7=create st_7
this.dw_1=create dw_1
this.em_fecha=create em_fecha
this.dw_11=create dw_11
this.dw_10=create dw_10
this.st_4=create st_4
this.st_3=create st_3
this.em_planilla=create em_planilla
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.sle_mensa=create sle_mensa
this.st_6=create st_6
this.Control[]={this.dw_2,&
this.st_7,&
this.dw_1,&
this.em_fecha,&
this.dw_11,&
this.dw_10,&
this.st_4,&
this.st_3,&
this.em_planilla,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.sle_mensa,&
this.st_6}
end on

on w_gene_archivo_edisag.destroy
destroy(this.dw_2)
destroy(this.st_7)
destroy(this.dw_1)
destroy(this.em_fecha)
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_planilla)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.sle_mensa)
destroy(this.st_6)
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

dw_2.GetChild("psag_codigo", idwc_tiposag)
idwc_tiposag.SetTransObject(sqlca)
idwc_tiposag.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"psag_codigo",4)

istr_mant.argumento[1]	=	String(gi_codexport,'000')
istr_mant.argumento[2]	=	String(gi_codplanta)
istr_mant.argumento[5] 	= 	'1'
istr_mant.argumento[6] 	= 	'4'

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_2 from datawindow within w_gene_archivo_edisag
integer x = 658
integer y = 736
integer width = 1170
integer height = 108
integer taborder = 70
string title = "none"
string dataobject = "dddw_tiposag"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[6] 	= 	data
end event

type st_7 from statictext within w_gene_archivo_edisag
integer x = 178
integer y = 752
integer width = 471
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_gene_archivo_edisag
boolean visible = false
integer x = 750
integer y = 1048
integer width = 686
integer height = 400
integer taborder = 80
boolean titlebar = true
string title = "none"
string dataobject = "dw_despacho_edit_sag"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type em_fecha from editmask within w_gene_archivo_edisag
integer x = 1138
integer y = 592
integer width = 402
integer height = 92
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type dw_11 from datawindow within w_gene_archivo_edisag
integer x = 416
integer y = 448
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

type dw_10 from datawindow within w_gene_archivo_edisag
integer x = 411
integer y = 308
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(Integer(data),'000')
idwc_planta.Retrieve(Integer(istr_mant.argumento[1]),1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type st_4 from statictext within w_gene_archivo_edisag
integer x = 178
integer y = 448
integer width = 402
integer height = 64
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_edisag
integer x = 178
integer y = 308
integer width = 402
integer height = 64
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type em_planilla from editmask within w_gene_archivo_edisag
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 658
integer y = 592
integer width = 443
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;IF ExistePlanilla(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type st_5 from statictext within w_gene_archivo_edisag
integer x = 87
integer y = 68
integer width = 1815
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Genera Archivo Plano Multipuerto"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_archivo_edisag
integer x = 178
integer y = 604
integer width = 471
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_edisag
integer x = 87
integer y = 224
integer width = 1815
integer height = 664
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_edisag
integer x = 1970
integer y = 856
integer width = 233
integer height = 196
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_edisag
integer x = 1970
integer y = 560
integer width = 233
integer height = 196
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;Integer	li_Respuesta

li_Respuesta =	MessageBox("Seleccione","Desea Informe de Revisión Previo a la Generación de Archivo",Exclamation!, OKCancel!,2)

IF  li_Respuesta = 1 THEN
   //
ELSE
	Parent.TriggerEvent("ue_guardar")
END IF

end event

type gb_2 from groupbox within w_gene_archivo_edisag
boolean visible = false
integer x = 1947
integer y = 512
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_1 from groupbox within w_gene_archivo_edisag
boolean visible = false
integer x = 1947
integer y = 820
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type sle_mensa from singlelineedit within w_gene_archivo_edisag
integer x = 119
integer y = 956
integer width = 1746
integer height = 92
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 33543637
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_gene_archivo_edisag
integer x = 87
integer y = 888
integer width = 1815
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

