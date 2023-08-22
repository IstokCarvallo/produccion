$PBExportHeader$w_carga_archivo_prefacturacion.srw
$PBExportComments$Ventana en donde se carga el Archivo Plano de Pre-Facturación
forward
global type w_carga_archivo_prefacturacion from window
end type
type dw_4 from datawindow within w_carga_archivo_prefacturacion
end type
type dw_3 from datawindow within w_carga_archivo_prefacturacion
end type
type cbx_carga from checkbox within w_carga_archivo_prefacturacion
end type
type dw_1 from datawindow within w_carga_archivo_prefacturacion
end type
type dw_2 from datawindow within w_carga_archivo_prefacturacion
end type
type em_fecha from editmask within w_carga_archivo_prefacturacion
end type
type st_8 from statictext within w_carga_archivo_prefacturacion
end type
type st_6 from statictext within w_carga_archivo_prefacturacion
end type
type dw_planta from datawindow within w_carga_archivo_prefacturacion
end type
type sle_mensa from singlelineedit within w_carga_archivo_prefacturacion
end type
type st_5 from statictext within w_carga_archivo_prefacturacion
end type
type pb_salir from picturebutton within w_carga_archivo_prefacturacion
end type
type pb_acepta from picturebutton within w_carga_archivo_prefacturacion
end type
type st_titulo from statictext within w_carga_archivo_prefacturacion
end type
type gb_2 from groupbox within w_carga_archivo_prefacturacion
end type
type gb_1 from groupbox within w_carga_archivo_prefacturacion
end type
type r_1 from rectangle within w_carga_archivo_prefacturacion
end type
type gb_5 from groupbox within w_carga_archivo_prefacturacion
end type
type st_3 from statictext within w_carga_archivo_prefacturacion
end type
end forward

global type w_carga_archivo_prefacturacion from window
integer x = 1074
integer y = 484
integer width = 2368
integer height = 1272
boolean titlebar = true
string title = "Generación de Procesos de Valorización"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
event ue_carga_detalle pbm_custom27
dw_4 dw_4
dw_3 dw_3
cbx_carga cbx_carga
dw_1 dw_1
dw_2 dw_2
em_fecha em_fecha
st_8 st_8
st_6 st_6
dw_planta dw_planta
sle_mensa sle_mensa
st_5 st_5
pb_salir pb_salir
pb_acepta pb_acepta
st_titulo st_titulo
gb_2 gb_2
gb_1 gb_1
r_1 r_1
gb_5 gb_5
st_3 st_3
end type
global w_carga_archivo_prefacturacion w_carga_archivo_prefacturacion

type variables
str_busqueda	istr_busq
Str_info			lstr_info

DataWindowChild	idwc_Planta

Integer    ii_TipoOrden
String     is_archivo
end variables

forward prototypes
public subroutine habilitagrabar ()
public subroutine cargadetalle (date ad_fecha, integer ai_planta)
end prototypes

event ue_carga_detalle;Integer	li_Archivo, li_Retorno, ll_fila, ll_fila2, li_planta
String	ls_datos, ls_tipo
date		ld_fecha

li_archivo	= FileOpen(is_archivo)

IF li_archivo < 0 THEN
	li_retorno = MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!,RetryCancel!)
	Message.DoubleParm = li_retorno
	RETURN 1
ELSE
	SetPointer(HourGlass!)

	DO WHILE FileRead(li_archivo, ls_datos) >= 0
		ls_Tipo				=	Mid(ls_datos,1,1)
		
		IF ls_Tipo  =  "1"   THEN
			ll_fila  =  dw_1.InsertRow(0)
			dw_1.Object.cdva_fecpro[ll_fila] = Date(Mid(ls_Datos,2,10))
			dw_1.Object.cdva_tipdoc[ll_fila] = Integer(Mid(ls_Datos,12,1))
			dw_1.Object.plde_codigo[ll_fila] = Integer(Mid(ls_Datos,13,4))
			dw_1.Object.prod_codigo[ll_fila] = Integer(Mid(ls_Datos,17,4))
			dw_1.Object.cdva_guisii[ll_fila] = Long(Mid(ls_Datos,21,8))

	ELSEIF ls_Tipo	=	"2"	THEN
				ll_fila2  =  dw_2.InsertRow(0)
			 	dw_2.Object.cvap_fecpro[ll_fila2] = Date(Mid(ls_datos,2,10))
			 	dw_2.Object.cvap_tipdoc[ll_fila2] = Integer(Mid(ls_datos,12,1))
				dw_2.Object.plde_codigo[ll_fila2] =	Integer(Mid(ls_datos,13,4))
				dw_2.Object.prod_codigo[ll_fila2] =	Integer(Mid(ls_datos,17,4))
				dw_2.Object.espe_codigo[ll_fila2] =	Integer(Mid(ls_datos,21,2))
				dw_2.Object.vari_codigo[ll_fila2] =	Integer(Mid(ls_datos,23,4))
				dw_2.Object.enva_tipoen[ll_fila2] =	Integer(Mid(ls_datos,27,1))
				dw_2.Object.enva_codigo[ll_fila2] =	Integer(Mid(ls_datos,28,3))
				dw_2.Object.cate_codigo[ll_fila2] =	Integer(Mid(ls_datos,31,3))			
				dw_2.Object.sepl_codigo[ll_fila2] = Integer(Mid(ls_datos,34,2))			
				dw_2.Object.cvap_totkil[ll_fila2] = Long(Mid(ls_datos,36,10))
				dw_2.Object.cvap_totcaj[ll_fila2] = Long(Mid(ls_datos,46,7))
				dw_2.Object.cvap_tipcam[ll_fila2] = Long(Mid(ls_datos,53,6))
				dw_2.Object.cvap_vaneus[ll_fila2] = Long(Mid(ls_datos,59,7))
				dw_2.Object.cvap_vanepe[ll_fila2] = Long(Mid(ls_datos,66,9))
				dw_2.Object.cvap_totnet[ll_fila2] = Long(Mid(ls_datos,75,12))
				dw_2.Object.cvap_valiva[ll_fila2] = Long(Mid(ls_datos,87,12))
				dw_2.Object.cvap_valtot[ll_fila2] = Long(Mid(ls_datos,99,12))
		END IF
	LOOP
	dw_1.Update()
	dw_2.Update()
END IF	
end event

public subroutine habilitagrabar ();
end subroutine

public subroutine cargadetalle (date ad_fecha, integer ai_planta);Integer	li_Archivo, li_Retorno, ll_fila, ll_fila2, li_planta
Long		ll_row
String	ls_datos, ls_tipo, ls_planta, ls_fecha
Date		ld_fecha

li_archivo	= FileOpen(is_archivo)

IF li_archivo < 0 THEN
	li_retorno = MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!,RetryCancel!)
	Message.DoubleParm = li_retorno
	RETURN 
ELSE 
	SetPointer(HourGlass!)
	
   sle_mensa.Text = "Cargando Archivo"

	DO WHILE FileRead(li_archivo, ls_datos) >= 0
		IF Mid(ls_datos,2,10) = String(ad_fecha) AND &
		   Mid(ls_datos,13,4) = String(ai_Planta,"0000") THEN
			
			ls_Tipo				=	Mid(ls_datos,1,1)
			
			IF ls_Tipo  =  "1"   THEN
				ll_fila  =  dw_1.InsertRow(0)
				dw_1.Object.cdva_fecpro[ll_fila] = Date(Mid(ls_Datos,2,10))
				dw_1.Object.cdva_tipdoc[ll_fila] = Integer(Mid(ls_Datos,12,1))
				dw_1.Object.plde_codigo[ll_fila] = Integer(Mid(ls_Datos,13,4))
				dw_1.Object.prod_codigo[ll_fila] = Integer(Mid(ls_Datos,17,4))
				dw_1.Object.cdva_guisii[ll_fila] = Long(Mid(ls_Datos,21,8))
	
			ELSEIF ls_Tipo	=	"2"	THEN
					 ll_fila2  =  dw_2.InsertRow(0)
					 dw_2.Object.cvap_fecpro[ll_fila2] = Date(Mid(ls_datos,2,10))
					 dw_2.Object.cvap_tipdoc[ll_fila2] = Integer(Mid(ls_datos,12,1))
					 dw_2.Object.plde_codigo[ll_fila2] = Integer(Mid(ls_datos,13,4))
					 dw_2.Object.prod_codigo[ll_fila2] = Integer(Mid(ls_datos,17,4))
					 dw_2.Object.espe_codigo[ll_fila2] = Integer(Mid(ls_datos,21,2))
					 dw_2.Object.vari_codigo[ll_fila2] = Integer(Mid(ls_datos,23,4))
					 dw_2.Object.enva_tipoen[ll_fila2] = Integer(Mid(ls_datos,27,1))
					 dw_2.Object.enva_codigo[ll_fila2] = Integer(Mid(ls_datos,28,3))
					 dw_2.Object.cate_codigo[ll_fila2] = Integer(Mid(ls_datos,31,3))			
					 dw_2.Object.sepl_codigo[ll_fila2] = Integer(Mid(ls_datos,34,2))			
					 dw_2.Object.cvap_totkil[ll_fila2] = Long(Mid(ls_datos,36,10))
					 dw_2.Object.cvap_totcaj[ll_fila2] = Long(Mid(ls_datos,46,7))
					 dw_2.Object.cvap_tipcam[ll_fila2] = Long(Mid(ls_datos,53,6))
					 dw_2.Object.cvap_vaneus[ll_fila2] = Long(Mid(ls_datos,59,7))
					 dw_2.Object.cvap_vanepe[ll_fila2] = Long(Mid(ls_datos,66,9))
					 dw_2.Object.cvap_totnet[ll_fila2] = Long(Mid(ls_datos,75,12))
					 dw_2.Object.cvap_valiva[ll_fila2] = Long(Mid(ls_datos,87,12))
					 dw_2.Object.cvap_valtot[ll_fila2] = Long(Mid(ls_datos,99,12))
			END IF
		ELSE
			MessageBox("Error","El Archivo no es el Correcto")
	   	sle_mensa.Text = "Archivo no Corresponde a los Parametros"						
			Return
		END IF 
	LOOP
	
	dw_3.Retrieve(ad_Fecha, ai_Planta, 0)
	Do While 1 <= dw_3.RowCount()
		dw_3.DeleteRow(1)
	Loop
	dw_3.Update()
	
	dw_4.Retrieve(ad_Fecha, ai_Planta, 0)
	Do While 1 <= dw_4.RowCount()
		dw_4.DeleteRow(1)
	Loop
	dw_4.Update()

	IF dw_1.Update() > 0 AND  dw_2.Update() > 0 THEN
   	sle_mensa.Text = "Archivo Cargado, Avise a Computación"

	END IF
END IF

end subroutine

on w_carga_archivo_prefacturacion.create
this.dw_4=create dw_4
this.dw_3=create dw_3
this.cbx_carga=create cbx_carga
this.dw_1=create dw_1
this.dw_2=create dw_2
this.em_fecha=create em_fecha
this.st_8=create st_8
this.st_6=create st_6
this.dw_planta=create dw_planta
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.pb_salir=create pb_salir
this.pb_acepta=create pb_acepta
this.st_titulo=create st_titulo
this.gb_2=create gb_2
this.gb_1=create gb_1
this.r_1=create r_1
this.gb_5=create gb_5
this.st_3=create st_3
this.Control[]={this.dw_4,&
this.dw_3,&
this.cbx_carga,&
this.dw_1,&
this.dw_2,&
this.em_fecha,&
this.st_8,&
this.st_6,&
this.dw_planta,&
this.sle_mensa,&
this.st_5,&
this.pb_salir,&
this.pb_acepta,&
this.st_titulo,&
this.gb_2,&
this.gb_1,&
this.r_1,&
this.gb_5,&
this.st_3}
end on

on w_carga_archivo_prefacturacion.destroy
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.cbx_carga)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.em_fecha)
destroy(this.st_8)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.pb_acepta)
destroy(this.st_titulo)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.r_1)
destroy(this.gb_5)
destroy(this.st_3)
end on

event open;X	=	0
Y	=	0
//Planta
dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)

IF idwc_planta.Retrieve(gi_codexport)=0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_planta.SetTransObject(SQLCA)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gstr_paramplanta.codigoplanta)
dw_planta.Enabled = False
dw_planta.Object.plde_codigo.BackGround.Color = RGB(192,192,192)

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(SQLCA)
dw_3.SetTransObject(SQLCA)
dw_4.SetTransObject(SQLCA)

end event

type dw_4 from datawindow within w_carga_archivo_prefacturacion
boolean visible = false
integer y = 1068
integer width = 443
integer height = 96
integer taborder = 110
string title = "none"
string dataobject = "dw_mues_spro_controlvalorizacion_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_carga_archivo_prefacturacion
boolean visible = false
integer y = 936
integer width = 443
integer height = 96
integer taborder = 120
string title = "none"
string dataobject = "dw_mues_spro_controldoctosvalori_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cbx_carga from checkbox within w_carga_archivo_prefacturacion
integer x = 571
integer y = 720
integer width = 882
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Carga de Archivo Plano"
end type

type dw_1 from datawindow within w_carga_archivo_prefacturacion
boolean visible = false
integer y = 672
integer width = 443
integer height = 96
integer taborder = 110
string title = "none"
string dataobject = "dw_mues_spro_controldoctosvalori_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_carga_archivo_prefacturacion
boolean visible = false
integer y = 804
integer width = 443
integer height = 96
integer taborder = 100
string title = "none"
string dataobject = "dw_mues_spro_controlvalorizacion_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type em_fecha from editmask within w_carga_archivo_prefacturacion
integer x = 617
integer y = 488
integer width = 389
integer height = 92
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

type st_8 from statictext within w_carga_archivo_prefacturacion
integer x = 165
integer y = 496
integer width = 439
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Mes a Generar"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_carga_archivo_prefacturacion
integer x = 165
integer y = 344
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_carga_archivo_prefacturacion
integer x = 617
integer y = 332
integer width = 882
integer height = 92
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;IF data <> '' THEN
	IF idwc_Planta.Find("plde_codigo = " + data, 1, idwc_Planta.RowCount()) = 0 THEN
		MessageBox("Atención", "Código de Planta indicado no ha sido~r" + &
						"creado en tabla respectiva.~r~rIngrese o seleccione" + &
						"otra Planta.")
		
		RETURN 1
	END IF
END IF
end event

type sle_mensa from singlelineedit within w_carga_archivo_prefacturacion
integer x = 142
integer y = 936
integer width = 1659
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_carga_archivo_prefacturacion
integer x = 82
integer y = 868
integer width = 1797
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_carga_archivo_prefacturacion
integer x = 2021
integer y = 668
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = right!
end type

event clicked;Close(Parent)
end event

type pb_acepta from picturebutton within w_carga_archivo_prefacturacion
event clicked pbm_bnclicked
integer x = 2021
integer y = 344
integer width = 155
integer height = 132
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = right!
end type

event clicked;Integer	li_Planta, li_Productor, li_mes, li_ano, &
         li_ConVariedad, li_ConCategoria, li_ConServicio, li_TipoInforme, li_valida
Long     ll_fila
String   ls_fecha, ls_directorio
Date 		ld_Hasta, ld_Desde


SetPointer(HourGlass!)

sle_mensa.text	=	""

li_Planta		=	dw_Planta.Object.plde_codigo[1]
IF IsNull(li_Planta) THEN
	MessageBox("Atención","Debe Seleccionar Planta",Exclamation!)
	
	RETURN
END IF

ls_fecha = '01/' + em_fecha.text
li_mes   =  integer(mid(ls_fecha,4,2)) + 1
li_ano   =  integer(mid(ls_fecha,7,4))

IF IsNull(li_mes) or li_mes = 1 or li_mes = 0 THEN
	MessageBox("Atención","Debe Seleccionar un Mes/Año de Proceso",Exclamation!)
	RETURN
END IF

IF IsNull(li_ano) or li_ano < 1930 or li_ano = 0  or li_ano > 2050 THEN
	MessageBox("Atención","Debe Seleccionar un Mes/Año de Proceso Valido",Exclamation!)
	RETURN
END IF

IF li_mes = 13 THEN
	li_mes = 1
   li_ano = li_ano + 1
END IF	

ld_desde = Date(ls_fecha)

ls_fecha = '01/' + string(li_mes,'00') + '/' + string(li_ano)

ld_hasta = Date(ls_fecha)

/* Rescata el Archivo Plano con sus Datos */
IF cbx_Carga.Checked THEN
	DO
		li_valida	= GetFileOpenName("Carga de Archivo", ls_directorio, is_archivo,&
												"Archivos Fruta Granel (*.TXT), *.TXT,Todos los Archivos (*.*), *.*")
		IF li_valida = 0 THEN
			pb_salir.SetFocus()
			RETURN
		ELSEIF li_valida = -1 THEN
			MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
			Message.DoubleParm = 1
		ELSE
			is_archivo	= ls_directorio
			Message.DoubleParm = 2

			cargadetalle(Date(ld_desde), li_planta)
	
		END IF
		
	LOOP WHILE Message.DoubleParm = 1
END IF


end event

type st_titulo from statictext within w_carga_archivo_prefacturacion
integer x = 82
integer y = 68
integer width = 1797
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Carga de Valorización"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type gb_2 from groupbox within w_carga_archivo_prefacturacion
integer x = 1961
integer y = 580
integer width = 274
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_1 from groupbox within w_carga_archivo_prefacturacion
integer x = 1961
integer y = 260
integer width = 274
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type r_1 from rectangle within w_carga_archivo_prefacturacion
long linecolor = 12632256
integer linethickness = 4
integer x = 773
integer y = 588
integer width = 165
integer height = 144
end type

type gb_5 from groupbox within w_carga_archivo_prefacturacion
integer x = 151
integer y = 648
integer width = 1659
integer height = 188
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Genera "
end type

type st_3 from statictext within w_carga_archivo_prefacturacion
integer x = 82
integer y = 224
integer width = 1797
integer height = 644
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

