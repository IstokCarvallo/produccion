$PBExportHeader$w_info_estiba_camarasbins.srw
forward
global type w_info_estiba_camarasbins from w_para_informes
end type
type st_1 from statictext within w_info_estiba_camarasbins
end type
type dw_cliente from datawindow within w_info_estiba_camarasbins
end type
type st_6 from statictext within w_info_estiba_camarasbins
end type
type dw_planta from datawindow within w_info_estiba_camarasbins
end type
type st_3 from statictext within w_info_estiba_camarasbins
end type
type dw_especie from datawindow within w_info_estiba_camarasbins
end type
type st_8 from statictext within w_info_estiba_camarasbins
end type
type dw_camara from datawindow within w_info_estiba_camarasbins
end type
type tit_peso from statictext within w_info_estiba_camarasbins
end type
type st_variedad from statictext within w_info_estiba_camarasbins
end type
type cbx_variedad from checkbox within w_info_estiba_camarasbins
end type
type em_variedad from editmask within w_info_estiba_camarasbins
end type
type cb_buscavariedad from commandbutton within w_info_estiba_camarasbins
end type
type sle_variedad from singlelineedit within w_info_estiba_camarasbins
end type
type st_5 from statictext within w_info_estiba_camarasbins
end type
type cbx_especie from checkbox within w_info_estiba_camarasbins
end type
type cbx_camaras from checkbox within w_info_estiba_camarasbins
end type
type gb_3 from groupbox within w_info_estiba_camarasbins
end type
type st_4 from statictext within w_info_estiba_camarasbins
end type
end forward

global type w_info_estiba_camarasbins from w_para_informes
integer x = 14
integer y = 32
integer width = 2624
integer height = 1572
string title = "Informe Estiba Cámaras"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
dw_especie dw_especie
st_8 st_8
dw_camara dw_camara
tit_peso tit_peso
st_variedad st_variedad
cbx_variedad cbx_variedad
em_variedad em_variedad
cb_buscavariedad cb_buscavariedad
sle_variedad sle_variedad
st_5 st_5
cbx_especie cbx_especie
cbx_camaras cbx_camaras
gb_3 gb_3
st_4 st_4
end type
global w_info_estiba_camarasbins w_info_estiba_camarasbins

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_camara, idwc_packing, idwc_pesoneto

String is_NomPlanta
end variables

forward prototypes
public function string get_clie_nombre (integer clie_codigo)
public function string get_plde_nombre (integer plde_codigo)
public function boolean existeespecie (integer especie)
public function boolean existepacking (ref string ls_columna)
public function boolean existeproductor (long ll_productor)
end prototypes

public function string get_clie_nombre (integer clie_codigo);string ls_nomcli

SELECT clie_nombre 	
	INTO :ls_nomcli FROM 
	dba.clientesprod WHERE 
	clie_codigo= :clie_codigo;

return ls_nomcli
end function

public function string get_plde_nombre (integer plde_codigo);string ls_nomplde

SELECT plde_nombre 	
	INTO :ls_nomplde FROM 
	dba.plantadesp WHERE 
	plde_codigo= :plde_codigo;

return ls_nomplde

end function

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existepacking (ref string ls_columna);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :ls_Columna;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = ls_columna	
	RETURN True 
END IF
end function

public function boolean existeproductor (long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_estiba_camarasbins.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.dw_especie=create dw_especie
this.st_8=create st_8
this.dw_camara=create dw_camara
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.cbx_variedad=create cbx_variedad
this.em_variedad=create em_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.sle_variedad=create sle_variedad
this.st_5=create st_5
this.cbx_especie=create cbx_especie
this.cbx_camaras=create cbx_camaras
this.gb_3=create gb_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.dw_especie
this.Control[iCurrent+7]=this.st_8
this.Control[iCurrent+8]=this.dw_camara
this.Control[iCurrent+9]=this.tit_peso
this.Control[iCurrent+10]=this.st_variedad
this.Control[iCurrent+11]=this.cbx_variedad
this.Control[iCurrent+12]=this.em_variedad
this.Control[iCurrent+13]=this.cb_buscavariedad
this.Control[iCurrent+14]=this.sle_variedad
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.cbx_especie
this.Control[iCurrent+17]=this.cbx_camaras
this.Control[iCurrent+18]=this.gb_3
this.Control[iCurrent+19]=this.st_4
end on

on w_info_estiba_camarasbins.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_8)
destroy(this.dw_camara)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.cbx_variedad)
destroy(this.em_variedad)
destroy(this.cb_buscavariedad)
destroy(this.sle_variedad)
destroy(this.st_5)
destroy(this.cbx_especie)
destroy(this.cbx_camaras)
destroy(this.gb_3)
destroy(this.st_4)
end on

event open;x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_camara.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SQLCA)
idwc_camara.Retrieve(gi_codplanta)
dw_camara.InsertRow(0)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	String(gi_CodPlanta)  	// planta						//	planta
istr_mant.argumento[3]	=	"-1"							//	camara
istr_mant.argumento[4]	= 	"-1" 							// especie
istr_mant.argumento[5]	=	'-1'                     // variedad
istr_mant.argumento[6]	=	'*'                     // embalaje
istr_mant.argumento[7]	=	'*'                     // calibre




end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_estiba_camarasbins
end type

type st_usuario from w_para_informes`st_usuario within w_info_estiba_camarasbins
end type

type st_temporada from w_para_informes`st_temporada within w_info_estiba_camarasbins
end type

type p_logo from w_para_informes`p_logo within w_info_estiba_camarasbins
end type

type st_titulo from w_para_informes`st_titulo within w_info_estiba_camarasbins
integer width = 1838
string text = "Informe Estiba Cámaras"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_estiba_camarasbins
string tag = "Imprimir Reporte"
integer x = 2245
integer y = 796
integer taborder = 100
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Integer	fila, li_cliente, li_planta, li_camara, li_especie, li_variedad
String	ls_embalaje, ls_calibre, ls_data_object

//valida seleccion de camara
IF IsNull(istr_mant.argumento[3]) THEN
 MessageBox("Atención", "Debe Ingresar Cámara Previamente ",Exclamation!)
	RETURN
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
li_camara		=	Integer(istr_mant.argumento[3])
li_especie		=	Integer(istr_mant.argumento[4])
li_variedad		=	Integer(istr_mant.argumento[5])

//selecciona el tipo de Informe
//IF rb_plano.checked = TRUE THEN
//	
	istr_info.titulo	= 'Informe Plano de Cámaras Por Bins'
   ls_data_object = "dw_info_estiba_camarasbins"
//ELSE
//
//	istr_info.titulo	= 'Informe '+rb_listado.text+' Cámaras'
//   ls_data_object = "dw_info_listado_estiba_camara"
//
//END IF	

SetPointer(HourGlass!)
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ls_data_object

vinf.dw_1.SetTransObject(sqlca)
fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, li_camara, li_especie, li_variedad)//,&
       							// ls_embalaje, ls_calibre)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("cliente.text = 'Cliente "+get_clie_nombre(li_cliente)+ "'")
	vinf.dw_1.Modify("planta.text = 'Planta "+get_plde_nombre(li_planta)+ "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_estiba_camarasbins
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2235
integer y = 1056
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 668
integer width = 462
integer height = 76
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_estiba_camarasbins
integer x = 631
integer y = 516
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(1)
	idwc_especie.Retrieve()
	dw_especie.SetItem(1, "espe_codigo", dw_especie.Object.espe_codigo[1])
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF

messagebox("",string(idwc_cliente.Retrieve(gi_CodExport)))
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 516
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_estiba_camarasbins
integer x = 631
integer y = 648
integer width = 960
integer height = 108
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantaclie"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[2]	= data

dw_camara.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SQLCA)
idwc_camara.Retrieve(gi_codplanta)
dw_camara.InsertRow(0)





end event

type st_3 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 1028
integer width = 421
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_estiba_camarasbins
integer x = 631
integer y = 1016
integer width = 878
integer height = 100
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExisteEspecie(Integer(data)) THEN
	istr_mant.argumento[4]	=	data
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	istr_mant.argumento[4]	=	String(gi_CodEspecie)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 792
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cámara"
boolean focusrectangle = false
end type

type dw_camara from datawindow within w_info_estiba_camarasbins
integer x = 622
integer y = 776
integer width = 992
integer height = 92
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_camaras_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[3]	=	data


end event

type tit_peso from statictext within w_info_estiba_camarasbins
boolean visible = false
integer x = 567
integer y = 1744
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 1160
integer width = 279
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_variedad from checkbox within w_info_estiba_camarasbins
integer x = 1742
integer y = 1152
integer width = 270
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	istr_mant.argumento[5]		=	'-1'
	
ELSE
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF
end event

type em_variedad from editmask within w_info_estiba_camarasbins
integer x = 631
integer y = 1144
integer width = 233
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Integer		li_especie, li_variedad
String		ls_Nombre

//li_especie	=	Integer(istr_mant.argumento[4]) // Especie
li_especie	=	dw_especie.Object.espe_codigo[1]
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dba.variedades
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variedades")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	sle_variedad.Text			=	ls_nombre
	istr_mant.argumento[5]	=	String(li_variedad)	
END IF
end event

type cb_buscavariedad from commandbutton within w_info_estiba_camarasbins
integer x = 882
integer y = 1148
integer width = 96
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente
lstr_busq.argum[2]	=	istr_mant.argumento[4] // Especie

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	em_variedad.SetFocus()
ELSE
	em_variedad.Text			=	lstr_busq.argum[4]
	sle_variedad.Text			=	lstr_busq.argum[5]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	
END IF

end event

type sle_variedad from singlelineedit within w_info_estiba_camarasbins
integer x = 983
integer y = 1144
integer width = 741
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_info_estiba_camarasbins
integer x = 242
integer y = 976
integer width = 1847
integer height = 328
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_especie from checkbox within w_info_estiba_camarasbins
integer x = 1742
integer y = 1028
integer width = 270
integer height = 76
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_especie.Enabled		=	False
	istr_mant.argumento[4]	=	'-1'
ELSE
	istr_mant.argumento[4]	=	'11'
	dw_especie.Enabled		=	True
	dw_especie.SetFocus()
END IF

end event

type cbx_camaras from checkbox within w_info_estiba_camarasbins
integer x = 1742
integer y = 784
integer width = 270
integer height = 76
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_camara.Enabled		=	False
	istr_mant.argumento[3]	=	'-1'
ELSE
	dw_camara.Enabled		=	True
	dw_camara.SetFocus()
END IF

end event

type gb_3 from groupbox within w_info_estiba_camarasbins
boolean visible = false
integer x = 174
integer y = 1412
integer width = 1614
integer height = 280
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_4 from statictext within w_info_estiba_camarasbins
integer x = 242
integer y = 440
integer width = 1847
integer height = 532
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

