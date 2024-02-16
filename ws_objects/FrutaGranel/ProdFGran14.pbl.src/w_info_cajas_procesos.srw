$PBExportHeader$w_info_cajas_procesos.srw
forward
global type w_info_cajas_procesos from w_para_informes
end type
type st_6 from statictext within w_info_cajas_procesos
end type
type st_1 from statictext within w_info_cajas_procesos
end type
type st_8 from statictext within w_info_cajas_procesos
end type
type st_3 from statictext within w_info_cajas_procesos
end type
type st_variedad from statictext within w_info_cajas_procesos
end type
type st_11 from statictext within w_info_cajas_procesos
end type
type st_embalaje from statictext within w_info_cajas_procesos
end type
type st_calidad from statictext within w_info_cajas_procesos
end type
type st_5 from statictext within w_info_cajas_procesos
end type
type uo_selplanta from uo_seleccion_plantas within w_info_cajas_procesos
end type
type uo_selproductor from uo_seleccion_productor within w_info_cajas_procesos
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_cajas_procesos
end type
type cbx_embalaje from checkbox within w_info_cajas_procesos
end type
type em_embalaje from editmask within w_info_cajas_procesos
end type
type cbx_calidad from checkbox within w_info_cajas_procesos
end type
type em_calidad from editmask within w_info_cajas_procesos
end type
type cb_buscaembalaje from commandbutton within w_info_cajas_procesos
end type
type st_2 from statictext within w_info_cajas_procesos
end type
type em_desde from editmask within w_info_cajas_procesos
end type
type st_7 from statictext within w_info_cajas_procesos
end type
type em_hasta from editmask within w_info_cajas_procesos
end type
type em_orden from editmask within w_info_cajas_procesos
end type
type cbx_ordentodo from checkbox within w_info_cajas_procesos
end type
type st_17 from statictext within w_info_cajas_procesos
end type
type cbx_todosfecha from checkbox within w_info_cajas_procesos
end type
type r_1 from rectangle within w_info_cajas_procesos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_cajas_procesos
end type
type cbx_consproc from checkbox within w_info_cajas_procesos
end type
type cbx_exporta from checkbox within w_info_cajas_procesos
end type
type sle_estado from singlelineedit within w_info_cajas_procesos
end type
type ddlb_tipo from dropdownlistbox within w_info_cajas_procesos
end type
type st_9 from statictext within w_info_cajas_procesos
end type
type st_12 from statictext within w_info_cajas_procesos
end type
type em_pallet from editmask within w_info_cajas_procesos
end type
type cbx_todospallet from checkbox within w_info_cajas_procesos
end type
type st_13 from statictext within w_info_cajas_procesos
end type
type rb_1 from radiobutton within w_info_cajas_procesos
end type
type rb_2 from radiobutton within w_info_cajas_procesos
end type
type cbx_conscajas from checkbox within w_info_cajas_procesos
end type
type st_4 from statictext within w_info_cajas_procesos
end type
type uo_selespecie from uo_seleccion_especie within w_info_cajas_procesos
end type
type rb_3 from radiobutton within w_info_cajas_procesos
end type
type st_10 from statictext within w_info_cajas_procesos
end type
type st_14 from statictext within w_info_cajas_procesos
end type
type dw_contratista from datawindow within w_info_cajas_procesos
end type
type cbx_cont from checkbox within w_info_cajas_procesos
end type
type cbx_det from checkbox within w_info_cajas_procesos
end type
type uo_seletiquetas from uo_seleccion_etiquetas within w_info_cajas_procesos
end type
end forward

global type w_info_cajas_procesos from w_para_informes
integer width = 4142
integer height = 2344
string title = "Informe de Cajas - Pallet / Pucho"
boolean minbox = false
boolean maxbox = false
st_6 st_6
st_1 st_1
st_8 st_8
st_3 st_3
st_variedad st_variedad
st_11 st_11
st_embalaje st_embalaje
st_calidad st_calidad
st_5 st_5
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cbx_calidad cbx_calidad
em_calidad em_calidad
cb_buscaembalaje cb_buscaembalaje
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
em_orden em_orden
cbx_ordentodo cbx_ordentodo
st_17 st_17
cbx_todosfecha cbx_todosfecha
r_1 r_1
uo_selcliente uo_selcliente
cbx_consproc cbx_consproc
cbx_exporta cbx_exporta
sle_estado sle_estado
ddlb_tipo ddlb_tipo
st_9 st_9
st_12 st_12
em_pallet em_pallet
cbx_todospallet cbx_todospallet
st_13 st_13
rb_1 rb_1
rb_2 rb_2
cbx_conscajas cbx_conscajas
st_4 st_4
uo_selespecie uo_selespecie
rb_3 rb_3
st_10 st_10
st_14 st_14
dw_contratista dw_contratista
cbx_cont cbx_cont
cbx_det cbx_det
uo_seletiquetas uo_seletiquetas
end type
global w_info_cajas_procesos w_info_cajas_procesos

type variables
str_mant				istr_mant

Integer				ii_nroorden, ii_tipoorden, ii_cliente, ii_informe

DataWindowChild	idwc_etiqueta
end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_etiqueta)
public function boolean existecontratista (integer ai_codigo)
end prototypes

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dba.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

public function boolean existecontratista (integer ai_codigo);Long		ll_count
Boolean	lb_Retorno = True

SELECT	Count()
	INTO	:ll_count
	FROM	dba.contratista
	WHERE	cont_codigo = :ai_codigo;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Contratista")
	lb_Retorno = False
ELSEIF ll_count = 0 THEN
	MessageBox("Atención", "No existe Contratista, ~nIngrese Otro.", Exclamation!)
	lb_Retorno = False
END IF	

Return lb_retorno
	
end function

on w_info_cajas_procesos.create
int iCurrent
call super::create
this.st_6=create st_6
this.st_1=create st_1
this.st_8=create st_8
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_11=create st_11
this.st_embalaje=create st_embalaje
this.st_calidad=create st_calidad
this.st_5=create st_5
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cbx_calidad=create cbx_calidad
this.em_calidad=create em_calidad
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.em_orden=create em_orden
this.cbx_ordentodo=create cbx_ordentodo
this.st_17=create st_17
this.cbx_todosfecha=create cbx_todosfecha
this.r_1=create r_1
this.uo_selcliente=create uo_selcliente
this.cbx_consproc=create cbx_consproc
this.cbx_exporta=create cbx_exporta
this.sle_estado=create sle_estado
this.ddlb_tipo=create ddlb_tipo
this.st_9=create st_9
this.st_12=create st_12
this.em_pallet=create em_pallet
this.cbx_todospallet=create cbx_todospallet
this.st_13=create st_13
this.rb_1=create rb_1
this.rb_2=create rb_2
this.cbx_conscajas=create cbx_conscajas
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.rb_3=create rb_3
this.st_10=create st_10
this.st_14=create st_14
this.dw_contratista=create dw_contratista
this.cbx_cont=create cbx_cont
this.cbx_det=create cbx_det
this.uo_seletiquetas=create uo_seletiquetas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_variedad
this.Control[iCurrent+6]=this.st_11
this.Control[iCurrent+7]=this.st_embalaje
this.Control[iCurrent+8]=this.st_calidad
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.uo_selplanta
this.Control[iCurrent+11]=this.uo_selproductor
this.Control[iCurrent+12]=this.uo_selvariedad
this.Control[iCurrent+13]=this.cbx_embalaje
this.Control[iCurrent+14]=this.em_embalaje
this.Control[iCurrent+15]=this.cbx_calidad
this.Control[iCurrent+16]=this.em_calidad
this.Control[iCurrent+17]=this.cb_buscaembalaje
this.Control[iCurrent+18]=this.st_2
this.Control[iCurrent+19]=this.em_desde
this.Control[iCurrent+20]=this.st_7
this.Control[iCurrent+21]=this.em_hasta
this.Control[iCurrent+22]=this.em_orden
this.Control[iCurrent+23]=this.cbx_ordentodo
this.Control[iCurrent+24]=this.st_17
this.Control[iCurrent+25]=this.cbx_todosfecha
this.Control[iCurrent+26]=this.r_1
this.Control[iCurrent+27]=this.uo_selcliente
this.Control[iCurrent+28]=this.cbx_consproc
this.Control[iCurrent+29]=this.cbx_exporta
this.Control[iCurrent+30]=this.sle_estado
this.Control[iCurrent+31]=this.ddlb_tipo
this.Control[iCurrent+32]=this.st_9
this.Control[iCurrent+33]=this.st_12
this.Control[iCurrent+34]=this.em_pallet
this.Control[iCurrent+35]=this.cbx_todospallet
this.Control[iCurrent+36]=this.st_13
this.Control[iCurrent+37]=this.rb_1
this.Control[iCurrent+38]=this.rb_2
this.Control[iCurrent+39]=this.cbx_conscajas
this.Control[iCurrent+40]=this.st_4
this.Control[iCurrent+41]=this.uo_selespecie
this.Control[iCurrent+42]=this.rb_3
this.Control[iCurrent+43]=this.st_10
this.Control[iCurrent+44]=this.st_14
this.Control[iCurrent+45]=this.dw_contratista
this.Control[iCurrent+46]=this.cbx_cont
this.Control[iCurrent+47]=this.cbx_det
this.Control[iCurrent+48]=this.uo_seletiquetas
end on

on w_info_cajas_procesos.destroy
call super::destroy
destroy(this.st_6)
destroy(this.st_1)
destroy(this.st_8)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_11)
destroy(this.st_embalaje)
destroy(this.st_calidad)
destroy(this.st_5)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cbx_calidad)
destroy(this.em_calidad)
destroy(this.cb_buscaembalaje)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.em_orden)
destroy(this.cbx_ordentodo)
destroy(this.st_17)
destroy(this.cbx_todosfecha)
destroy(this.r_1)
destroy(this.uo_selcliente)
destroy(this.cbx_consproc)
destroy(this.cbx_exporta)
destroy(this.sle_estado)
destroy(this.ddlb_tipo)
destroy(this.st_9)
destroy(this.st_12)
destroy(this.em_pallet)
destroy(this.cbx_todospallet)
destroy(this.st_13)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.cbx_conscajas)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.rb_3)
destroy(this.st_10)
destroy(this.st_14)
destroy(this.dw_contratista)
destroy(this.cbx_cont)
destroy(this.cbx_det)
destroy(this.uo_seletiquetas)
end on

event open;call super::open;Boolean lb_cerrar

IF IsNull(uo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)	THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelEtiquetas.Codigo) 	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
Else
	ii_informe 	= Integer(Message.StringParm)
	
	uo_SelEtiquetas.Seleccion(True, False)
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, True)
	uo_SelEspecie.Seleccion(True, True)
	uo_SelVariedad.Seleccion(True, True)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelProductor.Filtra(-1)
	
	cbx_todosfecha.TriggerEvent("clicked")
	
	em_desde.Text 									= 	String(RelativeDate(Today(), -365))
	em_hasta.Text 										= 	String(Today())
	ii_tipoorden 											=	-1
		
	ddlb_tipo.selectItem("Todos", 4)	
	cbx_todospallet.Enabled = False
	cbx_todospallet.Checked = True
	cbx_todospallet.TriggerEvent("clicked")
	
	rb_1.Checked = True
	rb_1.TriggerEvent("clicked")
	
	cbx_ordentodo.TriggerEvent("clicked")
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_cajas_procesos
integer x = 3625
integer y = 716
end type

type st_computador from w_para_informes`st_computador within w_info_cajas_procesos
end type

type st_usuario from w_para_informes`st_usuario within w_info_cajas_procesos
end type

type st_temporada from w_para_informes`st_temporada within w_info_cajas_procesos
end type

type p_logo from w_para_informes`p_logo within w_info_cajas_procesos
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_cajas_procesos
integer width = 3259
string text = "Informe de Cajas Embaladas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cajas_procesos
integer x = 3625
integer y = 1564
integer taborder = 100
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer		li_consfecha, 	li_conscajas, li_contratista
String			ls_embalaje, 	ls_calidad, 	ls_Archivo,	ls_ruta
Long			ll_nroorden, 	fila, 			ll_pallet
Date			ld_desde, 		ld_hasta

SetPointer(HourGlass!)

IF cbx_exporta.Checked THEN
	sle_estado.text	=	"Iniciando Exportación"
END IF

istr_info.titulo	= "INFORME DE CAJAS"
istr_info.copias	= 1

IF cbx_cont.Checked THEN
	li_contratista	= 	-1
ELSE
	dw_contratista.AcceptText()
	li_contratista = 	dw_contratista.Object.cont_codigo[1]
END IF

IF cbx_ordentodo.Checked THEN
	ll_nroOrden	= 	-1
	IF cbx_consproc.Checked THEN	ll_nroOrden	= 	-9
ELSE
	ll_nroOrden 	= 	Long(em_orden.Text)
END IF

IF cbx_todospallet.Checked	THEN
	ll_pallet	=	-1
ELSE
	ll_pallet	=	Long(em_pallet.Text)
END IF

IF cbx_conscajas.Checked THEN	li_conscajas	= 	-9

IF cbx_embalaje.Checked THEN
	ls_embalaje 	= '-1'
ELSE
	ls_embalaje 	= 	em_embalaje.Text
END IF

IF cbx_calidad.Checked THEN
	ls_calidad 	= '-1'
ELSE
	ls_calidad 	= 	em_calidad.Text
END IF

ld_desde			=	Date(em_desde.Text)
ld_hasta			=	Date(em_hasta.Text)

OpenWithParm(vinf, istr_info)

IF ii_informe = 1 THEN
	IF rb_3.Checked THEN
		vinf.dw_1.DataObject = "dw_info_cajas_variedad"
	ELSE
		vinf.dw_1.DataObject = "dw_info_cajas"
	END IF
ELSE
	IF NOT cbx_exporta.Checked THEN
		vinf.dw_1.DataObject = "dw_infopallets_spro_palletfruta"
	ELSE
		vinf.dw_1.DataObject = "dw_infopallets_spro_palletfruta"
	END IF
END IF


vinf.dw_1.SetTransObject(sqlca)

IF ii_informe = 1 THEN
	fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_Selplanta.codigo, ii_tipoorden,ll_nroorden,&
								 uo_SelProductor.codigo,uo_SelEspecie.codigo,uo_SelVariedad.codigo, ls_embalaje,&
								 ls_calidad,ld_desde, ld_hasta, li_conscajas, li_contratista)
								 
ELSE
	fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_Selplanta.codigo, ii_tipoorden,ll_nroorden,&
								 uo_SelProductor.codigo,uo_SelEspecie.codigo,uo_SelVariedad.codigo, ls_embalaje,&
								 ls_calidad,ld_desde, ld_hasta,ll_pallet, li_conscajas)
								 
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("Cliente.text = '" + uo_SelCliente.Nombre + "'")
	
	IF cbx_det.Checked THEN
		vinf.dw_1.Modify('Dw_1.Visible = ' + '1')
	ELSE
		vinf.dw_1.Modify('Dw_1.Visible = ' + '0')
	END IF
	
	IF cbx_exporta.Checked THEN
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
		ls_Archivo				= '\Cajas Embaladas del ' + String(ld_desde) + " al " + String(ld_desde) + '.xls'
		vinf.dw_1.SaveAs(ls_ruta + ls_archivo, Excel!,True) 
		sle_estado.text		= 	"Listo"
		Close(vinf)
	ELSE
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_cajas_procesos
integer x = 3625
integer y = 1824
integer taborder = 170
fontcharset fontcharset = ansi!
end type

type st_6 from statictext within w_info_cajas_procesos
integer x = 302
integer y = 672
integer width = 233
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

type st_1 from statictext within w_info_cajas_procesos
integer x = 302
integer y = 844
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_cajas_procesos
integer x = 302
integer y = 1048
integer width = 329
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_cajas_procesos
integer x = 302
integer y = 1248
integer width = 270
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_cajas_procesos
integer x = 1870
integer y = 672
integer width = 302
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_cajas_procesos
integer x = 1870
integer y = 852
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_info_cajas_procesos
integer x = 1870
integer y = 1048
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_calidad from statictext within w_info_cajas_procesos
integer x = 1874
integer y = 1248
integer width = 256
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_cajas_procesos
integer x = 1792
integer y = 560
integer width = 1714
integer height = 804
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_info_cajas_procesos
event destroy ( )
integer x = 809
integer y = 760
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_cajas_procesos
event destroy ( )
integer x = 809
integer y = 960
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_cajas_procesos
event destroy ( )
integer x = 2231
integer y = 572
integer taborder = 20
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_embalaje from checkbox within w_info_cajas_procesos
integer x = 2231
integer y = 964
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled		=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]	=	'0'
ELSE
	em_embalaje.Enabled		=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_cajas_procesos
integer x = 2235
integer y = 1032
integer width = 297
integer height = 92
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	uo_selcliente.Codigo // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido para el cliente " + String(li_cliente) + ".~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cbx_calidad from checkbox within w_info_cajas_procesos
integer x = 2231
integer y = 1156
integer width = 297
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[8]		=	'Z'
	istr_mant.argumento[18]		=	'0'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type em_calidad from editmask within w_info_cajas_procesos
integer x = 2235
integer y = 1228
integer width = 297
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

li_cliente	=	uo_selcliente.Codigo // Cliente
li_especie	=	uo_selespecie.Codigo // Especie
li_variedad	=	uo_selvariedad.Codigo // Variedad
ls_calibre	=	This.Text

//ls_calibre	=	Trim(ls_calibre) + Fill(" ",3 - Len(ls_calibre))

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.variecalibre
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad
	AND	vaca_calibr	=	:ls_calibre;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variecalibre")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Calibre no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[8]	=	ls_calibre
	em_calidad.Text			=	ls_calibre
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_cajas_procesos
integer x = 2537
integer y = 1028
integer width = 119
integer height = 100
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

type st_2 from statictext within w_info_cajas_procesos
integer x = 434
integer y = 1848
integer width = 425
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_cajas_procesos
integer x = 942
integer y = 1832
integer width = 375
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[9]	=	This.Text
end event

type st_7 from statictext within w_info_cajas_procesos
integer x = 1376
integer y = 1848
integer width = 279
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_cajas_procesos
integer x = 1701
integer y = 1832
integer width = 375
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[10]	=	This.Text
end event

type em_orden from editmask within w_info_cajas_procesos
integer x = 942
integer y = 1640
integer width = 375
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string minmax = "~~8"
end type

event modified;ii_nroorden	=	Long(This.Text)
end event

type cbx_ordentodo from checkbox within w_info_cajas_procesos
integer x = 2213
integer y = 1652
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_orden.Enabled										=	False
	em_orden.Text 											=	''
	ii_nroorden												=	-1
	cbx_consproc.Enabled									=	True
ELSE
	em_orden.Enabled										=	True
	cbx_consproc.Enabled									=	False
	ii_nroorden												=	Long(em_orden.Text)
	em_orden.SetFocus()
END IF
end event

type st_17 from statictext within w_info_cajas_procesos
integer x = 434
integer y = 1656
integer width = 443
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
string text = "Orden Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_cajas_procesos
integer x = 2213
integer y = 1844
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Text 		= String(RelativeDate(Today(), -365))
	em_hasta.Text 		= String(Today())
	em_desde.enabled	= False
	em_hasta.enabled		= False
ELSE
	em_desde.enabled	= True
	em_hasta.enabled		= True
END IF
end event

type r_1 from rectangle within w_info_cajas_procesos
integer linethickness = 4
long fillcolor = 33543637
integer x = 2574
integer y = 1692
integer width = 229
integer height = 200
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_cajas_procesos
integer x = 809
integer y = 576
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_consproc from checkbox within w_info_cajas_procesos
integer x = 2697
integer y = 1648
integer width = 603
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
end type

event clicked;IF THIS.Checked THEN
	cbx_ordentodo.Checked 	= 	True
	cbx_ordentodo.TriggerEvent("clicked")
	
	cbx_ordentodo.Enabled 		= 	False
ELSE
	cbx_ordentodo.Enabled 		= 	True
END IF
end event

type cbx_exporta from checkbox within w_info_cajas_procesos
integer x = 2715
integer y = 1156
integer width = 507
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Exportar a Excel"
end type

event clicked;IF This.Checked THEN
	sle_estado.Visible	=	True
	sle_estado.Text		=	"En Espera"
ELSE
	sle_estado.Visible	=	True
END IF
end event

type sle_estado from singlelineedit within w_info_cajas_procesos
integer x = 2624
integer y = 1228
integer width = 699
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 30586022
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type ddlb_tipo from dropdownlistbox within w_info_cajas_procesos
integer x = 1591
integer y = 1640
integer width = 480
integer height = 400
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
string item[] = {"Proceso","Reproceso","Reembalaje","Todos"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
	CASE 1
		ii_tipoorden = 4
	CASE 2
		ii_tipoorden = 8
	CASE 3
		ii_tipoorden = 7
	CASE ELSE
		ii_tipoorden = -1
END CHOOSE
end event

type st_9 from statictext within w_info_cajas_procesos
integer x = 1376
integer y = 1656
integer width = 210
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
string text = "Tipo"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_cajas_procesos
integer x = 434
integer y = 2056
integer width = 425
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
string text = "Nro Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_pallet from editmask within w_info_cajas_procesos
integer x = 942
integer y = 2040
integer width = 375
integer height = 96
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string minmax = "~~8"
end type

event modified;ii_nroorden	=	Long(This.Text)
end event

type cbx_todospallet from checkbox within w_info_cajas_procesos
integer x = 2213
integer y = 2052
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_pallet.Text 		= ""
	em_pallet.enabled		= False
ELSE
	em_pallet.enabled		= True
END IF
end event

type st_13 from statictext within w_info_cajas_procesos
integer x = 247
integer y = 412
integer width = 3259
integer height = 148
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_cajas_procesos
integer x = 809
integer y = 448
integer width = 681
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cajas por Proceso"
boolean checked = true
end type

event clicked;ii_informe						=	1

cbx_todospallet.Enabled 	= 	FALSE
cbx_todospallet.Checked 	= 	TRUE

cbx_consproc.Enabled 		= 	FALSE
cbx_consproc.Checked 		= 	FALSE
cbx_ordentodo.Checked 		= 	TRUE
cbx_todosfecha.Checked 	= 	TRUE

cbx_ordentodo.TriggerEvent("clicked")
cbx_todosfecha.TriggerEvent("clicked")
cbx_todospallet.TriggerEvent("clicked")

dw_contratista.Enabled		=	True
cbx_cont.Enabled				=	True
cbx_det.Enabled				=	True

cbx_cont.Checked				=	True
cbx_det.Checked				=	True
cbx_cont.TriggerEvent(Clicked!)
end event

type rb_2 from radiobutton within w_info_cajas_procesos
integer x = 2231
integer y = 448
integer width = 983
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pallet / Pucho Por Proceso"
end type

event clicked;ii_informe						=	2

cbx_todospallet.Enabled 	= 	True
cbx_todospallet.Checked 	= 	True

cbx_consproc.Enabled 		= 	True

cbx_todospallet.TriggerEvent("clicked")

dw_contratista.Enabled		=	False
cbx_cont.Enabled				=	False
cbx_det.Enabled				=	False

cbx_cont.Checked				=	True
cbx_det.Checked				=	False
cbx_cont.TriggerEvent(Clicked!)
end event

type cbx_conscajas from checkbox within w_info_cajas_procesos
integer x = 2697
integer y = 2052
integer width = 603
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Cajas"
end type

type st_4 from statictext within w_info_cajas_procesos
integer x = 247
integer y = 560
integer width = 1545
integer height = 804
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_cajas_procesos
integer x = 805
integer y = 1152
integer taborder = 80
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled		=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
END CHOOSE
end event

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type rb_3 from radiobutton within w_info_cajas_procesos
integer x = 1504
integer y = 448
integer width = 695
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cajas por Variedad"
end type

event clicked;ii_informe						=	1

cbx_todospallet.Enabled 		= 	FALSE
cbx_todospallet.Checked 	= 	TRUE

cbx_consproc.Enabled 		= 	FALSE
cbx_consproc.Checked 		= 	FALSE
cbx_ordentodo.Checked 		= 	TRUE
cbx_todosfecha.Checked 	= 	TRUE

cbx_ordentodo.TriggerEvent("clicked")
cbx_todosfecha.TriggerEvent("clicked")
cbx_todospallet.TriggerEvent("clicked")

dw_contratista.Enabled		=	False
cbx_cont.Enabled				=	False
cbx_det.Enabled				=	False

cbx_cont.Checked				=	True
cbx_det.Checked				=	False
cbx_cont.TriggerEvent(Clicked!)
end event

type st_10 from statictext within w_info_cajas_procesos
integer x = 247
integer y = 1364
integer width = 3259
integer height = 836
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_cajas_procesos
integer x = 434
integer y = 1460
integer width = 443
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
string text = "Contratista"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_contratista from datawindow within w_info_cajas_procesos
integer x = 937
integer y = 1432
integer width = 923
integer height = 96
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_contratistas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_null

SetNull(li_null)

IF Not existecontratista(Integer(Data)) THEN
	This.SetItem(Row,'cont_codigo', li_Null)
	RETURN 1
END IF	
end event

type cbx_cont from checkbox within w_info_cajas_procesos
integer x = 2213
integer y = 1448
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_contratista.Enabled = False
	dw_contratista.Reset()
	dw_contratista.SetTransObject(SQLCA)
	dw_contratista.InsertRow(0)
ELSE
	dw_contratista.Reset()
	dw_contratista.SetTransObject(SQLCA)
	dw_contratista.InsertRow(0)
	dw_contratista.Enabled = True
END IF
end event

type cbx_det from checkbox within w_info_cajas_procesos
integer x = 2702
integer y = 1448
integer width = 649
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Detalle Kilos Vaciados"
boolean checked = true
end type

type uo_seletiquetas from uo_seleccion_etiquetas within w_info_cajas_procesos
integer x = 2231
integer y = 760
integer taborder = 60
boolean bringtotop = true
end type

on uo_seletiquetas.destroy
call uo_seleccion_etiquetas::destroy
end on

