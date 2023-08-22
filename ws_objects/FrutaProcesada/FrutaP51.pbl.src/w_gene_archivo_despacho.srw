$PBExportHeader$w_gene_archivo_despacho.srw
forward
global type w_gene_archivo_despacho from window
end type
type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_despacho
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_despacho
end type
type ddlb_1 from dropdownlistbox within w_gene_archivo_despacho
end type
type st_7 from statictext within w_gene_archivo_despacho
end type
type cbx_validadestino from checkbox within w_gene_archivo_despacho
end type
type cbx_var from checkbox within w_gene_archivo_despacho
end type
type st_4 from statictext within w_gene_archivo_despacho
end type
type st_3 from statictext within w_gene_archivo_despacho
end type
type sle_mensa from singlelineedit within w_gene_archivo_despacho
end type
type em_planilla from editmask within w_gene_archivo_despacho
end type
type st_5 from statictext within w_gene_archivo_despacho
end type
type dw_22 from datawindow within w_gene_archivo_despacho
end type
type st_2 from statictext within w_gene_archivo_despacho
end type
type pb_salir from picturebutton within w_gene_archivo_despacho
end type
type pb_grabar from picturebutton within w_gene_archivo_despacho
end type
type st_6 from statictext within w_gene_archivo_despacho
end type
type dw_1 from datawindow within w_gene_archivo_despacho
end type
type dw_2 from datawindow within w_gene_archivo_despacho
end type
type em_fecha from editmask within w_gene_archivo_despacho
end type
type st_1 from statictext within w_gene_archivo_despacho
end type
end forward

global type w_gene_archivo_despacho from window
integer width = 2359
integer height = 1200
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
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
ddlb_1 ddlb_1
st_7 st_7
cbx_validadestino cbx_validadestino
cbx_var cbx_var
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_planilla em_planilla
st_5 st_5
dw_22 dw_22
st_2 st_2
pb_salir pb_salir
pb_grabar pb_grabar
st_6 st_6
dw_1 dw_1
dw_2 dw_2
em_fecha em_fecha
st_1 st_1
end type
global w_gene_archivo_despacho w_gene_archivo_despacho

type variables
Date					id_FechaAcceso
Time					it_HoraAcceso
integer  				ii_var
String 				is_tipoplanilla
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public function boolean masdeundestino (integer al_planilla)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_destinoins
Integer		li_PldSag, li_region
String			ls_Archivo, ls_Registro

dw_2.reset()
dw_1.reset()

If Not DirectoryExists(gs_disco+":\GeneradosSAG") Then
	MessageBox('Error', 'El directorio para la generacion de archivo: ' + gs_disco+":\GeneradosSAG" + &
					"~n~nNo existe, favor revisar o comunicarse con informatica, para activar directorio.", StopSign!, Ok!)
	Return
End If

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)

dw_1.Reset()

SELECT plde_region INTO :li_region
FROM dbo.plantadesp
WHERE plde_codigo=:uo_SelPlantas.Codigo;

ll_Numero	= Long(em_planilla.text)

ll_Filas		= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, ll_Numero,ii_var,is_tipoplanilla)

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")
ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_planilla.SetFocus()
ELSE
	dw_2.SetSort('paen_numero')
   	dw_2.Sort()

	li_PldSag		 =	dw_2.Object.plde_codpla[1]
	ls_Registro 	 =	String(li_PldSag,'0000')
	ll_destinoins	 = dw_2.Object.destinoins[1] 
//	ls_Registro		+=	String( (dw_2.Object.destinoins[1]) , '000')
	IF Isnull(ll_destinoins) THEN ll_destinoins = 0
	ls_Registro		+=	String( ll_destinoins , '000')
//	ls_Registro		+=	String(dw_2.Object.inpd_fechai[1], 'YYYYMMDD')
	ls_Registro		+=	String(dw_2.Object.defe_fecdes[1], 'YYYYMMDD')
	ls_Registro		+= String(dw_2.Object.cantpallets[1], '0000')

	ll_FilaDet	=	dw_1.InsertRow(0)
	dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	
	FOR ll_Fila = 1 TO ll_Filas
		ls_Registro 	=	f_ClienteRotulado(uo_SelCliente.Codigo)
		ls_Registro	+=	String(dw_2.Object.paen_numero[ll_Fila], '0000000')
		ls_Registro	+=	String(dw_2.Object.cantcajas[ll_Fila], '0000')
		ls_Registro	+=	dw_2.Object.espe_codsag[ll_Fila]
		
		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	NEXT
	
	ls_Registro	= '&&'

	ll_FilaDet	=	dw_1.InsertRow(0)
	dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	
	ll_Numero = dw_2.Object.defe_plasag[1]
	
	IF li_region <> 4 THEN
		ls_Archivo	= String(li_PldSag,'000') + String(ll_Numero,'00000') + ".DES"
	ELSE	
		ls_Archivo	= String(li_PldSag,'0000') + String(ll_Numero,'00000') + ".DES"
	END IF	

	IF dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 THEN
		MessageBox("Atención","No se pudo generar el archivo "+ls_Archivo)
	ELSE
		sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
	END IF
	
	dw_2.Reset()

END IF

em_planilla.SetFocus()
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeplanilla (long al_planilla);Integer	li_tiposa
Date		ld_fecha

IF (al_planilla <> 0) OR uo_SelPlantas.Codigo = 0 THEN
	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN
		WHERE	plde_codigo =	:uo_SelPlantas.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_plasag	=	:al_planilla
		AND	defe_nturno =  :is_tipoplanilla;	
	
	SELECT Min(defe_tiposa)
		INTO	:li_tiposa
		FROM	dbo.DESPAFRIGOEN
		WHERE	plde_codigo =	:uo_SelPlantas.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_plasag	=	:al_planilla
		AND	defe_nturno =  :is_tipoplanilla;
				
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
	ELSEIF IsNull(li_tiposa) THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
//	ELSEIF li_tiposa = 11 THEN
//		MessageBox("Atención", "Planilla S.A.G. Corresponde a Interplanta.~r~rIngrese otro Número.", &
//						Exclamation!, Ok!)
//		pb_grabar.Enabled	= False
//		em_planilla.SetFocus()
//		RETURN False		
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

public function boolean masdeundestino (integer al_planilla);Integer	li_variosdestinos

IF (al_planilla <> 0) OR uo_SelPlantas.Codigo = 0 THEN
	
	SELECT Count(distinct dest_codigo)
		INTO	:li_variosdestinos
		FROM	dbo.DESPAFRIGOEN as DE, dbo.PALLETENCAB as PE, dbo.DESPAFRIGODE as DD
		WHERE	de.plde_codigo =	:uo_SelPlantas.Codigo
		AND	de.clie_codigo	=	:uo_SelCliente.Codigo
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
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSEIF li_variosdestinos > 1 THEN
				MessageBox("Atención", "Planilla S.A.G. Incluye Mas de un Destino.", Exclamation!, Ok!)
				pb_grabar.Enabled	= False
				em_planilla.SetFocus()
				RETURN False
	ELSE
		pb_grabar.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_gene_archivo_despacho.create
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
this.ddlb_1=create ddlb_1
this.st_7=create st_7
this.cbx_validadestino=create cbx_validadestino
this.cbx_var=create cbx_var
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_planilla=create em_planilla
this.st_5=create st_5
this.dw_22=create dw_22
this.st_2=create st_2
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_6=create st_6
this.dw_1=create dw_1
this.dw_2=create dw_2
this.em_fecha=create em_fecha
this.st_1=create st_1
this.Control[]={this.uo_selplantas,&
this.uo_selcliente,&
this.ddlb_1,&
this.st_7,&
this.cbx_validadestino,&
this.cbx_var,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_planilla,&
this.st_5,&
this.dw_22,&
this.st_2,&
this.pb_salir,&
this.pb_grabar,&
this.st_6,&
this.dw_1,&
this.dw_2,&
this.em_fecha,&
this.st_1}
end on

on w_gene_archivo_despacho.destroy
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
destroy(this.ddlb_1)
destroy(this.st_7)
destroy(this.cbx_validadestino)
destroy(this.cbx_var)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_planilla)
destroy(this.st_5)
destroy(this.dw_22)
destroy(this.st_2)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.em_fecha)
destroy(this.st_1)
end on

event open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	x	=	0
	y	=	0
	This.Icon	=	Gstr_apl.Icono
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
		
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
								
	ddlb_1.SelectItem(Integer(1))
	
	is_tipoplanilla = '1'
	
	//ii_var	=	1
	IF gi_vari_rotulada = 1 THEN
		cbx_var.Checked	= True
		cbx_var.Enabled	=	False
	ELSE
		cbx_var.Checked	= False
		cbx_var.Enabled	=	True
	END IF	
	ii_var	= gi_vari_rotulada
End If
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_despacho
event destroy ( )
integer x = 576
integer y = 344
integer height = 92
integer taborder = 50
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_despacho
event destroy ( )
integer x = 576
integer y = 244
integer height = 92
integer taborder = 10
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type ddlb_1 from dropdownlistbox within w_gene_archivo_despacho
integer x = 576
integer y = 464
integer width = 1275
integer height = 400
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string item[] = {"1. Productos Agríc. de Export. Certificados","2. Productos Agr.Export. Cert. (USDA)","3. Fruta a ser Fumigada en U.S.A.","4. Fumigados","5. Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;is_tipoplanilla	=	String(index)
end event

type st_7 from statictext within w_gene_archivo_despacho
integer x = 133
integer y = 472
integer width = 402
integer height = 64
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type cbx_validadestino from checkbox within w_gene_archivo_despacho
integer x = 576
integer y = 576
integer width = 613
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Valida Destino"
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type cbx_var from checkbox within w_gene_archivo_despacho
integer x = 576
integer y = 752
integer width = 480
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rot."
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type st_4 from statictext within w_gene_archivo_despacho
integer x = 133
integer y = 356
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

type st_3 from statictext within w_gene_archivo_despacho
integer x = 133
integer y = 256
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

type sle_mensa from singlelineedit within w_gene_archivo_despacho
integer x = 128
integer y = 912
integer width = 1719
integer height = 132
boolean bringtotop = true
integer textsize = -12
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

type em_planilla from editmask within w_gene_archivo_despacho
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 581
integer y = 644
integer width = 443
integer height = 92
integer taborder = 60
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
	END IF
	
	IF cbx_validadestino.Checked THEN
		IF MasdeunDestino(Long(This.Text)) = False THEN
			em_planilla.Text	=	''
			em_planilla.SetFocus()
		END IF
	END IF		
END IF	
end event

type st_5 from statictext within w_gene_archivo_despacho
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
string text = "Genera Archivo Plano Anexo Despacho Inspeccionado"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_22 from datawindow within w_gene_archivo_despacho
boolean visible = false
integer x = 791
integer y = 1148
integer width = 265
integer height = 172
integer taborder = 100
string dataobject = "dw_gene_archivo_despacho"
end type

event clicked;This.Print()
end event

type st_2 from statictext within w_gene_archivo_despacho
integer x = 133
integer y = 652
integer width = 448
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_despacho
integer x = 1979
integer y = 864
integer width = 302
integer height = 244
integer taborder = 80
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

type pb_grabar from picturebutton within w_gene_archivo_despacho
integer x = 1975
integer y = 584
integer width = 302
integer height = 244
integer taborder = 70
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

type st_6 from statictext within w_gene_archivo_despacho
integer x = 82
integer y = 864
integer width = 1815
integer height = 224
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_gene_archivo_despacho
boolean visible = false
integer x = 475
integer y = 1124
integer width = 265
integer height = 172
boolean bringtotop = true
string dataobject = "dw_gene_archivo_saam_plano"
end type

type dw_2 from datawindow within w_gene_archivo_despacho
boolean visible = false
integer x = 128
integer y = 1136
integer width = 265
integer height = 172
integer taborder = 110
boolean bringtotop = true
string dataobject = "dw_gene_archivo_embarque_inspec"
end type

event clicked;//This.Print()
end event

type em_fecha from editmask within w_gene_archivo_despacho
integer x = 1097
integer y = 648
integer width = 471
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "none"
boolean border = false
alignment alignment = center!
boolean displayonly = true
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_1 from statictext within w_gene_archivo_despacho
integer x = 82
integer y = 224
integer width = 1815
integer height = 640
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

