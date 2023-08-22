$PBExportHeader$w_info_recepcion_resumida.srw
forward
global type w_info_recepcion_resumida from w_para_informes
end type
type st_4 from statictext within w_info_recepcion_resumida
end type
type st_1 from statictext within w_info_recepcion_resumida
end type
type st_2 from statictext within w_info_recepcion_resumida
end type
type em_desde from editmask within w_info_recepcion_resumida
end type
type st_6 from statictext within w_info_recepcion_resumida
end type
type st_3 from statictext within w_info_recepcion_resumida
end type
type st_7 from statictext within w_info_recepcion_resumida
end type
type em_hasta from editmask within w_info_recepcion_resumida
end type
type st_8 from statictext within w_info_recepcion_resumida
end type
type st_5 from statictext within w_info_recepcion_resumida
end type
type cbx_peso from checkbox within w_info_recepcion_resumida
end type
type tit_peso from statictext within w_info_recepcion_resumida
end type
type st_variedad from statictext within w_info_recepcion_resumida
end type
type st_embalaje from statictext within w_info_recepcion_resumida
end type
type cbx_embalaje from checkbox within w_info_recepcion_resumida
end type
type cb_buscaembalaje from commandbutton within w_info_recepcion_resumida
end type
type cbx_consembalaje from checkbox within w_info_recepcion_resumida
end type
type st_11 from statictext within w_info_recepcion_resumida
end type
type st_calidad from statictext within w_info_recepcion_resumida
end type
type cbx_calidad from checkbox within w_info_recepcion_resumida
end type
type cbx_conscalidad from checkbox within w_info_recepcion_resumida
end type
type gb_13 from groupbox within w_info_recepcion_resumida
end type
type st_18 from statictext within w_info_recepcion_resumida
end type
type st_9 from statictext within w_info_recepcion_resumida
end type
type cbx_fecemb from checkbox within w_info_recepcion_resumida
end type
type gb_4 from groupbox within w_info_recepcion_resumida
end type
type st_12 from statictext within w_info_recepcion_resumida
end type
type cbx_infopacking from checkbox within w_info_recepcion_resumida
end type
type st_14 from statictext within w_info_recepcion_resumida
end type
type dw_pesoneto from datawindow within w_info_recepcion_resumida
end type
type st_15 from statictext within w_info_recepcion_resumida
end type
type cbx_todosfru from checkbox within w_info_recepcion_resumida
end type
type cbx_consfru from checkbox within w_info_recepcion_resumida
end type
type dw_frurecep from datawindow within w_info_recepcion_resumida
end type
type em_calidad from editmask within w_info_recepcion_resumida
end type
type cbx_predio from checkbox within w_info_recepcion_resumida
end type
type uo_selespecie from uo_seleccion_especie within w_info_recepcion_resumida
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_recepcion_resumida
end type
type cbx_varirotula from checkbox within w_info_recepcion_resumida
end type
type cbx_1 from checkbox within w_info_recepcion_resumida
end type
type em_norden from editmask within w_info_recepcion_resumida
end type
type st_21 from statictext within w_info_recepcion_resumida
end type
type cbx_2 from checkbox within w_info_recepcion_resumida
end type
type gb_5 from groupbox within w_info_recepcion_resumida
end type
type st_10 from statictext within w_info_recepcion_resumida
end type
type gb_6 from groupbox within w_info_recepcion_resumida
end type
type st_13 from statictext within w_info_recepcion_resumida
end type
type em_guia from editmask within w_info_recepcion_resumida
end type
type st_16 from statictext within w_info_recepcion_resumida
end type
type cbx_3 from checkbox within w_info_recepcion_resumida
end type
type cbx_4 from checkbox within w_info_recepcion_resumida
end type
type st_17 from statictext within w_info_recepcion_resumida
end type
type uo_seltipoproductor from uo_seleccion_tipoproductor within w_info_recepcion_resumida
end type
type em_embalaje from singlelineedit within w_info_recepcion_resumida
end type
type st_19 from statictext within w_info_recepcion_resumida
end type
type uo_selcate from uo_seleccion_categoria within w_info_recepcion_resumida
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_recepcion_resumida
end type
type uo_seletiqueta from uo_seleccion_etiquetas within w_info_recepcion_resumida
end type
type uo_selpacking from uo_seleccion_plantas within w_info_recepcion_resumida
end type
type uo_selplantas from uo_seleccion_plantas within w_info_recepcion_resumida
end type
type uo_selzonas from uo_seleccion_zonas within w_info_recepcion_resumida
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_recepcion_resumida
end type
end forward

global type w_info_recepcion_resumida from w_para_informes
integer x = 14
integer y = 32
integer width = 4050
integer height = 2504
string title = "RECEPCIONES RESUMIDAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
st_6 st_6
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_8 st_8
st_5 st_5
cbx_peso cbx_peso
tit_peso tit_peso
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_consembalaje cbx_consembalaje
st_11 st_11
st_calidad st_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
gb_13 gb_13
st_18 st_18
st_9 st_9
cbx_fecemb cbx_fecemb
gb_4 gb_4
st_12 st_12
cbx_infopacking cbx_infopacking
st_14 st_14
dw_pesoneto dw_pesoneto
st_15 st_15
cbx_todosfru cbx_todosfru
cbx_consfru cbx_consfru
dw_frurecep dw_frurecep
em_calidad em_calidad
cbx_predio cbx_predio
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_1 cbx_1
em_norden em_norden
st_21 st_21
cbx_2 cbx_2
gb_5 gb_5
st_10 st_10
gb_6 gb_6
st_13 st_13
em_guia em_guia
st_16 st_16
cbx_3 cbx_3
cbx_4 cbx_4
st_17 st_17
uo_seltipoproductor uo_seltipoproductor
em_embalaje em_embalaje
st_19 st_19
uo_selcate uo_selcate
uo_selproductor uo_selproductor
uo_seletiqueta uo_seletiqueta
uo_selpacking uo_selpacking
uo_selplantas uo_selplantas
uo_selzonas uo_selzonas
uo_selcliente uo_selcliente
end type
global w_info_recepcion_resumida w_info_recepcion_resumida

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_pesoneto, idwc_fruta

Long		ll_norden, il_guia

uo_frutarecepcion		iuo_frutarecepcion
uo_calibre				iuo_calibre
end variables

forward prototypes
public function string buscdescfruta (integer codigo)
end prototypes

public function string buscdescfruta (integer codigo);String	ls_descri


    SELECT frre_descri  
    INTO :ls_descri  
    FROM dbo.recfruprocee as re, dbo.frutarecibida as fr 
   WHERE re.frre_codigo = fr.frre_codigo and  
         re.frre_codigo = :codigo ;

		
RETURN ls_descri

end function

on w_info_recepcion_resumida.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_11=create st_11
this.st_calidad=create st_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.gb_13=create gb_13
this.st_18=create st_18
this.st_9=create st_9
this.cbx_fecemb=create cbx_fecemb
this.gb_4=create gb_4
this.st_12=create st_12
this.cbx_infopacking=create cbx_infopacking
this.st_14=create st_14
this.dw_pesoneto=create dw_pesoneto
this.st_15=create st_15
this.cbx_todosfru=create cbx_todosfru
this.cbx_consfru=create cbx_consfru
this.dw_frurecep=create dw_frurecep
this.em_calidad=create em_calidad
this.cbx_predio=create cbx_predio
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_1=create cbx_1
this.em_norden=create em_norden
this.st_21=create st_21
this.cbx_2=create cbx_2
this.gb_5=create gb_5
this.st_10=create st_10
this.gb_6=create gb_6
this.st_13=create st_13
this.em_guia=create em_guia
this.st_16=create st_16
this.cbx_3=create cbx_3
this.cbx_4=create cbx_4
this.st_17=create st_17
this.uo_seltipoproductor=create uo_seltipoproductor
this.em_embalaje=create em_embalaje
this.st_19=create st_19
this.uo_selcate=create uo_selcate
this.uo_selproductor=create uo_selproductor
this.uo_seletiqueta=create uo_seletiqueta
this.uo_selpacking=create uo_selpacking
this.uo_selplantas=create uo_selplantas
this.uo_selzonas=create uo_selzonas
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_7
this.Control[iCurrent+8]=this.em_hasta
this.Control[iCurrent+9]=this.st_8
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.cbx_peso
this.Control[iCurrent+12]=this.tit_peso
this.Control[iCurrent+13]=this.st_variedad
this.Control[iCurrent+14]=this.st_embalaje
this.Control[iCurrent+15]=this.cbx_embalaje
this.Control[iCurrent+16]=this.cb_buscaembalaje
this.Control[iCurrent+17]=this.cbx_consembalaje
this.Control[iCurrent+18]=this.st_11
this.Control[iCurrent+19]=this.st_calidad
this.Control[iCurrent+20]=this.cbx_calidad
this.Control[iCurrent+21]=this.cbx_conscalidad
this.Control[iCurrent+22]=this.gb_13
this.Control[iCurrent+23]=this.st_18
this.Control[iCurrent+24]=this.st_9
this.Control[iCurrent+25]=this.cbx_fecemb
this.Control[iCurrent+26]=this.gb_4
this.Control[iCurrent+27]=this.st_12
this.Control[iCurrent+28]=this.cbx_infopacking
this.Control[iCurrent+29]=this.st_14
this.Control[iCurrent+30]=this.dw_pesoneto
this.Control[iCurrent+31]=this.st_15
this.Control[iCurrent+32]=this.cbx_todosfru
this.Control[iCurrent+33]=this.cbx_consfru
this.Control[iCurrent+34]=this.dw_frurecep
this.Control[iCurrent+35]=this.em_calidad
this.Control[iCurrent+36]=this.cbx_predio
this.Control[iCurrent+37]=this.uo_selespecie
this.Control[iCurrent+38]=this.uo_selvariedad
this.Control[iCurrent+39]=this.cbx_varirotula
this.Control[iCurrent+40]=this.cbx_1
this.Control[iCurrent+41]=this.em_norden
this.Control[iCurrent+42]=this.st_21
this.Control[iCurrent+43]=this.cbx_2
this.Control[iCurrent+44]=this.gb_5
this.Control[iCurrent+45]=this.st_10
this.Control[iCurrent+46]=this.gb_6
this.Control[iCurrent+47]=this.st_13
this.Control[iCurrent+48]=this.em_guia
this.Control[iCurrent+49]=this.st_16
this.Control[iCurrent+50]=this.cbx_3
this.Control[iCurrent+51]=this.cbx_4
this.Control[iCurrent+52]=this.st_17
this.Control[iCurrent+53]=this.uo_seltipoproductor
this.Control[iCurrent+54]=this.em_embalaje
this.Control[iCurrent+55]=this.st_19
this.Control[iCurrent+56]=this.uo_selcate
this.Control[iCurrent+57]=this.uo_selproductor
this.Control[iCurrent+58]=this.uo_seletiqueta
this.Control[iCurrent+59]=this.uo_selpacking
this.Control[iCurrent+60]=this.uo_selplantas
this.Control[iCurrent+61]=this.uo_selzonas
this.Control[iCurrent+62]=this.uo_selcliente
end on

on w_info_recepcion_resumida.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_11)
destroy(this.st_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.gb_13)
destroy(this.st_18)
destroy(this.st_9)
destroy(this.cbx_fecemb)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.cbx_infopacking)
destroy(this.st_14)
destroy(this.dw_pesoneto)
destroy(this.st_15)
destroy(this.cbx_todosfru)
destroy(this.cbx_consfru)
destroy(this.dw_frurecep)
destroy(this.em_calidad)
destroy(this.cbx_predio)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_1)
destroy(this.em_norden)
destroy(this.st_21)
destroy(this.cbx_2)
destroy(this.gb_5)
destroy(this.st_10)
destroy(this.gb_6)
destroy(this.st_13)
destroy(this.em_guia)
destroy(this.st_16)
destroy(this.cbx_3)
destroy(this.cbx_4)
destroy(this.st_17)
destroy(this.uo_seltipoproductor)
destroy(this.em_embalaje)
destroy(this.st_19)
destroy(this.uo_selcate)
destroy(this.uo_selproductor)
destroy(this.uo_seletiqueta)
destroy(this.uo_selpacking)
destroy(this.uo_selplantas)
destroy(this.uo_selzonas)
destroy(this.uo_selcliente)
end on

event resize;//
end event

event open;Boolean lb_Cerrar

If IsNull(uo_SelEtiqueta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCate.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelTipoProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPacking.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelZonas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(True,False)
	uo_SelEtiqueta.Seleccion(True, True)
	uo_SelEspecie.Seleccion(True,True)
	uo_SelCate.Seleccion(True, True)
	uo_SelProductor.Seleccion(True,True)
	uo_SelTipoProductor.Seleccion(True,True)
	uo_SelVariedad.Seleccion(True,True)
	uo_SelPacking.Seleccion(True,True)
	uo_SelPlantas.Seleccion(True,True)
	uo_SelZonas.Seleccion(True,True)
	
	uo_SelPlantas.Filtra(1)
	uo_SelPacking.Filtra(2)
	uo_SelCliente.Todos(False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	iuo_frutarecepcion		=	Create uo_frutarecepcion		
	iuo_calibre   			=	Create uo_calibre
	
	dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
	idwc_pesoneto.SetTransObject(SQLCA)
	idwc_pesoneto.Retrieve()
	dw_pesoneto.InsertRow(0)
	dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(553648127))
	
	dw_frurecep.GetChild("frre_codigo", idwc_fruta)
	idwc_fruta.SetTransObject(SQLCA)
	idwc_fruta.Retrieve()
	dw_frurecep.InsertRow(0)
	
	dw_frurecep.enabled	=	false
	
	em_desde.Text				=	String(RelativeDate(Today(), -365))
	em_hasta.Text				=	String(Today())
	istr_mant.argumento[6]  =  "Z"							//	embalaje
	istr_mant.argumento[8]  =  "Z"							//	calidad
	istr_mant.argumento[9]	= 	em_desde.Text				//	fecha inicio
	istr_mant.argumento[10]	=	em_hasta.Text				//	fecha final
	istr_mant.argumento[11] =  "1"							//	peso
	istr_mant.argumento[13] =  "1"							//	Consolidado Planta
	istr_mant.argumento[14] =  "1"							//	Consolidado Productor
	istr_mant.argumento[16] =  "1"							//	Consolidado Embalaje
	istr_mant.argumento[17] =  "1"							//	Consolidado Etiqueta
	istr_mant.argumento[18] =  "1"							//	Consolidado Calidad
	istr_mant.argumento[33] =  "0"							//	Todos Packing
	ll_norden					=	-9								// Orden de proceso
	il_guia					=	-9
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_recepcion_resumida
integer x = 3657
integer y = 1068
end type

type st_computador from w_para_informes`st_computador within w_info_recepcion_resumida
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepcion_resumida
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepcion_resumida
end type

type p_logo from w_para_informes`p_logo within w_info_recepcion_resumida
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepcion_resumida
integer width = 3259
string text = "Informe de Recepciones Resumidas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepcion_resumida
integer x = 3579
integer y = 1748
integer taborder = 160
integer weight = 400
fontcharset fontcharset = ansi!
boolean italic = true
boolean underline = true
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_consplanta,li_consproductor, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_fruta, li_Agru, li_varirotula = 0, li_emba, li_emba2
Date		ld_desde, ld_hasta
String		texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_embalaje, ls_calidad,&
         	ls_null, ls_descri, ls_lista, ls_construyelike1, ls_construye, ls_string, ls_construyelike
Long		ll_productor

SetNull(ls_null)

ls_embalaje	=	istr_mant.argumento[6]+','

If cbx_infopacking.Checked Then
	istr_info.titulo	= 'INFORME DE RECEPCIONES RESUMIDAS POR PACKING'

	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_recepcion_packing_enc"
ElseIf cbx_predio.Checked Then
	istr_info.titulo	= 'INFORME DE RECEPCIONES RESUMIDAS POR PREDIO'
	
	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_recepciones_resumidas_precua"
	li_Agru	=	0
Else
	istr_info.titulo	= 'INFORME DE RECEPCIONES RESUMIDAS'
	
	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_recepciones_resumidas"
   li_Agru	=	1
END If

li_emba = len(ls_embalaje)

FOR li_emba2 = 1 TO li_emba
	ls_string = mid(ls_embalaje,li_emba2,1)
	
	If ls_string <> ',' THEN
		ls_construye = ls_construye+ls_string
	ELSE
		If ls_construyelike1 = '' THEN
			ls_construyelike1 = ' emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			ls_construye = ''
		ELSE	
			IF ls_construyelike = '' THEN
				ls_construyelike = ls_construyelike1 +' or emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			ELSE
				ls_construyelike = ls_construyelike +' or emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			END IF
			ls_construye = ''
		END IF	
	END IF	
NEXT	

IF ls_construyelike = '' THEN
	ls_construyelike = ls_construyelike1
END IF

If cbx_varirotula.Checked Then li_varirotula = 1

ls_lista = uo_selproductor.Lista

ll_productor	=	Long(istr_mant.argumento[4])
ls_embalaje		=	istr_mant.argumento[6]
ls_calidad		=	istr_mant.argumento[8]
ld_desde			=	Date(istr_mant.argumento[9])
ld_hasta			=	Date(istr_mant.argumento[10])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bultos"
	istr_mant.argumento[11]	=	"1"
ELSE
	istr_mant.argumento[11]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas							=	"Base " + istr_mant.argumento[11] 
END IF

If uo_SelPlantas.Codigo = -9 Then li_consplanta		=	-9
li_consproductor	=	Integer(istr_mant.argumento[14])
li_consembalaje	=	Integer(istr_mant.argumento[16])
If uo_selEtiqueta.Codigo = -9 Then  li_consetiqueta = -9
li_conscalidad		=	Integer(istr_mant.argumento[18])
If uo_SelPacking.Codigo = -9 Then li_conspacking	=	-9

IF cbx_fecemb.checked THEN
	istr_mant.argumento[41] = "0"
ELSE
	istr_mant.argumento[41] = "1"
END IF

//Caracteristica de Recepción
IF cbx_consfru.Checked THEN
	li_fruta 	= -9
	ls_descri	= 'CONSOLIDADA'
ELSEIF cbx_todosfru.checked  THEN
	li_fruta  =   -1
	ls_descri	=	'TODAS'
ELSE
	li_fruta	=	dw_frurecep.Object.frre_codigo[1]
	IF IsNull(li_fruta) OR li_fruta = 0 THEN
	   MessageBox("Atención","Debe Seleccionar una Carácteristica Previamente",Exclamation!)
		dw_frurecep.setfocus()
		RETURN
	ELSE
		ls_descri	=	buscdescfruta(li_fruta)
	END IF 
END IF

IF cbx_embalaje.Checked OR cbx_consembalaje.Checked THEN
	ls_construyelike = "'Z' = 'Z'"
END IF

vinf.dw_1.SetTransObject(sqlca)

IF cbx_infopacking.Checked = True THEN
	fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecie.Codigo, uo_SelPlantas.Codigo, &
							uo_SelVariedad.Codigo, ls_embalaje, uo_SelEtiqueta.Codigo, ls_calidad,&
							ld_desde, ld_hasta, Dec(istr_mant.argumento[11]), li_consplanta,&
							li_consembalaje, li_consetiqueta, li_conscalidad,&
							uo_SelPacking.Codigo, li_conspacking, uo_SelZonas.Codigo,Integer(istr_mant.argumento[41]),&
							li_fruta, li_varirotula,ls_lista,ls_construyelike,uo_SelCate.Codigo) 
ELSE
	fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelEspecie.Codigo, uo_SelPlantas.Codigo, &
							 uo_SelVariedad.Codigo, ls_embalaje, uo_SelEtiqueta.Codigo, ls_calidad,&
							 ld_desde, ld_hasta, Dec(istr_mant.argumento[11]), li_consplanta, &
							 li_consembalaje, li_consetiqueta, li_conscalidad,&
							 uo_SelPacking.Codigo,li_conspacking, uo_SelZonas.Codigo,Integer(istr_mant.argumento[41]),&
							 li_fruta,li_Agru, li_varirotula,ll_norden,il_guia,uo_SelTipoProductor.Codigo,&
							 ls_lista,ls_construyelike,uo_SelCate.Codigo)	
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("Cliente.text = '" + uo_SelCliente.Nombre + "'")
		vinf.dw_1.Modify("Fruta.text = '" + ls_descri + "'")
		vinf.dw_1.Modify("Especie.text = '" + uo_SelEspecie.Nombre + "'")
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepcion_resumida
integer x = 3579
integer y = 2024
integer taborder = 170
end type

type st_4 from statictext within w_info_recepcion_resumida
integer x = 256
integer y = 628
integer width = 1545
integer height = 1204
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_recepcion_resumida
integer x = 302
integer y = 912
integer width = 443
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

type st_2 from statictext within w_info_recepcion_resumida
integer x = 293
integer y = 2128
integer width = 384
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_recepcion_resumida
integer x = 658
integer y = 2108
integer width = 416
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[9]	=	This.Text
end event

type st_6 from statictext within w_info_recepcion_resumida
integer x = 1125
integer y = 532
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

type st_3 from statictext within w_info_recepcion_resumida
integer x = 302
integer y = 1680
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_recepcion_resumida
integer x = 293
integer y = 2240
integer width = 384
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_recepcion_resumida
integer x = 658
integer y = 2216
integer width = 416
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[10]	=	This.Text
end event

type st_8 from statictext within w_info_recepcion_resumida
integer x = 302
integer y = 1176
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_recepcion_resumida
integer x = 1806
integer y = 628
integer width = 1714
integer height = 1204
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_recepcion_resumida
integer x = 315
integer y = 1904
integer width = 631
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
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type tit_peso from statictext within w_info_recepcion_resumida
integer x = 965
integer y = 1912
integer width = 155
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
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_recepcion_resumida
integer x = 1883
integer y = 924
integer width = 315
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

type st_embalaje from statictext within w_info_recepcion_resumida
integer x = 1883
integer y = 1104
integer width = 302
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

type cbx_embalaje from checkbox within w_info_recepcion_resumida
integer x = 2414
integer y = 1016
integer width = 416
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
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_recepcion_resumida
integer x = 2720
integer y = 1092
integer width = 110
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[6]	=	lstr_busq.argum[2]
END IF
end event

type cbx_consembalaje from checkbox within w_info_recepcion_resumida
integer x = 2926
integer y = 1020
integer width = 535
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'1'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type st_11 from statictext within w_info_recepcion_resumida
integer x = 1883
integer y = 724
integer width = 311
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

type st_calidad from statictext within w_info_recepcion_resumida
integer x = 1893
integer y = 1284
integer width = 270
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
string text = "Calidad"
boolean focusrectangle = false
end type

type cbx_calidad from checkbox within w_info_recepcion_resumida
integer x = 2414
integer y = 1200
integer width = 311
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
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[8]		=	'Z'
	istr_mant.argumento[18]		=	'0'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_recepcion_resumida
integer x = 2926
integer y = 1200
integer width = 485
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'1'
ELSE
	istr_mant.argumento[18]	=	'0'
END IF

end event

type gb_13 from groupbox within w_info_recepcion_resumida
integer x = 288
integer y = 1844
integer width = 1563
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_18 from statictext within w_info_recepcion_resumida
integer x = 302
integer y = 1500
integer width = 443
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
string text = "Packing"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_recepcion_resumida
integer x = 302
integer y = 732
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
string text = "Zona"
boolean focusrectangle = false
end type

type cbx_fecemb from checkbox within w_info_recepcion_resumida
integer x = 1115
integer y = 2176
integer width = 434
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
boolean checked = true
end type

type gb_4 from groupbox within w_info_recepcion_resumida
integer x = 1929
integer y = 1840
integer width = 1563
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_12 from statictext within w_info_recepcion_resumida
integer x = 261
integer y = 420
integer width = 3259
integer height = 208
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_infopacking from checkbox within w_info_recepcion_resumida
integer x = 2459
integer y = 1884
integer width = 1010
integer height = 80
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe por Packing (Crosstab)"
end type

event clicked;IF This.Checked THEN
	cbx_predio.Checked = False
	em_guia.Enabled = False
	em_guia.Text = ''
	il_guia = -9
	cbx_3.Checked = True
	cbx_3.Enabled = False
	cbx_4.Checked = True
	cbx_4.Enabled = False
ELSE
	cbx_4.Enabled = True
END IF	
end event

type st_14 from statictext within w_info_recepcion_resumida
integer x = 256
integer y = 1836
integer width = 3264
integer height = 224
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_pesoneto from datawindow within w_info_recepcion_resumida
integer x = 1111
integer y = 1896
integer width = 695
integer height = 100
integer taborder = 120
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type st_15 from statictext within w_info_recepcion_resumida
integer x = 1874
integer y = 1460
integer width = 539
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
string text = "Caract.Recepción"
boolean focusrectangle = false
end type

type cbx_todosfru from checkbox within w_info_recepcion_resumida
integer x = 2414
integer y = 1372
integer width = 320
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
end type

event clicked;Integer li_null
SetNull(li_null)

call super::clicked;IF This.Checked THEN
   cbx_consfru.Checked   =  false
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	dw_frurecep.Enabled		=	True
	dw_frurecep.SetFocus()
END IF
end event

type cbx_consfru from checkbox within w_info_recepcion_resumida
integer x = 2926
integer y = 1372
integer width = 485
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;Integer li_Null
SetNull(li_null)

IF This.Checked THEN
	cbx_todosfru.Checked		=	False       
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	cbx_todosfru.Checked		=	true      
	dw_frurecep.Enabled		=	false
END IF
end event

type dw_frurecep from datawindow within w_info_recepcion_resumida
integer x = 2414
integer y = 1436
integer width = 581
integer height = 100
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_frutarecep"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula

IF NOT iuo_frutarecepcion.existe(Integer(data),True,sqlca) THEN
	This.SetItem(1, "frre_codigo", li_nula)
	RETURN 1
ELSE
	istr_mant.argumento[9] = data
END IF
end event

type em_calidad from editmask within w_info_recepcion_resumida
integer x = 2414
integer y = 1264
integer width = 311
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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

IF This.Text <> '' THEN
	li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[8]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	


end event

type cbx_predio from checkbox within w_info_recepcion_resumida
integer x = 1984
integer y = 1916
integer width = 402
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
string text = "Por Predio"
end type

event clicked;IF This.Checked THEN
	cbx_infopacking.Checked = False
END IF	
end event

type uo_selespecie from uo_seleccion_especie within w_info_recepcion_resumida
event destroy ( )
integer x = 741
integer y = 1612
integer height = 168
integer taborder = 70
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_recepcion_resumida
event destroy ( )
integer x = 2414
integer y = 836
integer width = 910
integer height = 172
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_recepcion_resumida
integer x = 2459
integer y = 1952
integer width = 677
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
string text = "Variedad Rotulada"
end type

type cbx_1 from checkbox within w_info_recepcion_resumida
integer x = 1609
integer y = 2112
integer width = 288
integer height = 68
integer taborder = 80
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -1
ELSE
	em_norden.Enabled = True
	ll_norden = Long(em_norden.Text)
END IF
end event

type em_norden from editmask within w_info_recepcion_resumida
integer x = 1970
integer y = 2204
integer width = 416
integer height = 84
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;ll_norden = Long(this.text)
end event

type st_21 from statictext within w_info_recepcion_resumida
integer x = 1605
integer y = 2216
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_info_recepcion_resumida
integer x = 1947
integer y = 2112
integer width = 443
integer height = 68
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
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -9
	cbx_1.Checked = True
	cbx_1.Enabled = False
ELSE
	em_norden.Enabled = False
	ll_norden = -1
	cbx_1.Checked = True
	cbx_1.Enabled = True
END IF
end event

type gb_5 from groupbox within w_info_recepcion_resumida
integer x = 1600
integer y = 2068
integer width = 827
integer height = 256
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
end type

type st_10 from statictext within w_info_recepcion_resumida
integer x = 256
integer y = 2060
integer width = 1321
integer height = 288
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_6 from groupbox within w_info_recepcion_resumida
integer x = 2441
integer y = 2064
integer width = 1056
integer height = 256
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
end type

type st_13 from statictext within w_info_recepcion_resumida
integer x = 1577
integer y = 2060
integer width = 1943
integer height = 288
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_guia from editmask within w_info_recepcion_resumida
integer x = 2821
integer y = 2204
integer width = 430
integer height = 84
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;il_guia = Long(this.text)
end event

type st_16 from statictext within w_info_recepcion_resumida
integer x = 2523
integer y = 2216
integer width = 279
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Guía SII"
boolean focusrectangle = false
end type

type cbx_3 from checkbox within w_info_recepcion_resumida
integer x = 2615
integer y = 2112
integer width = 288
integer height = 68
integer taborder = 80
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_guia.Enabled = False
	em_guia.Text = ''
	il_guia = -1
ELSE
	em_guia.Enabled = True
	il_guia = Long(em_guia.Text)
END IF
end event

type cbx_4 from checkbox within w_info_recepcion_resumida
integer x = 2999
integer y = 2112
integer width = 443
integer height = 68
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
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_guia.Enabled = False
	em_guia.Text = ''
	il_guia = -9
	cbx_3.Checked = True
	cbx_3.Enabled = False
ELSE
	em_guia.Enabled = False
	il_guia = -1
	cbx_3.Checked = True
	cbx_3.Enabled = True
END IF
end event

type st_17 from statictext within w_info_recepcion_resumida
integer x = 302
integer y = 1328
integer width = 443
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
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type uo_seltipoproductor from uo_seleccion_tipoproductor within w_info_recepcion_resumida
integer x = 741
integer y = 1264
integer height = 160
integer taborder = 280
boolean bringtotop = true
end type

on uo_seltipoproductor.destroy
call uo_seleccion_tipoproductor::destroy
end on

type em_embalaje from singlelineedit within w_info_recepcion_resumida
integer x = 2414
integer y = 1096
integer width = 311
integer height = 84
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
//	This.SetFocus()
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
//		"Ingrese o seleccione otro Código.")
//	This.SetFocus()
//ELSE
	istr_mant.argumento[6]	=	ls_embalaje
	istr_mant.argumento[16]	=	'0'
//END IF
end event

type st_19 from statictext within w_info_recepcion_resumida
integer x = 1874
integer y = 1632
integer width = 320
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
string text = "Categoria"
boolean focusrectangle = false
end type

type uo_selcate from uo_seleccion_categoria within w_info_recepcion_resumida
integer x = 2414
integer y = 1568
integer width = 910
integer height = 156
integer taborder = 340
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_recepcion_resumida
integer x = 741
integer y = 988
integer height = 268
integer taborder = 130
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

type uo_seletiqueta from uo_seleccion_etiquetas within w_info_recepcion_resumida
event destroy ( )
integer x = 2414
integer y = 652
integer taborder = 80
boolean bringtotop = true
end type

on uo_seletiqueta.destroy
call uo_seleccion_etiquetas::destroy
end on

type uo_selpacking from uo_seleccion_plantas within w_info_recepcion_resumida
event destroy ( )
integer x = 741
integer y = 1428
integer taborder = 120
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_recepcion_resumida
event destroy ( )
integer x = 741
integer y = 812
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selzonas from uo_seleccion_zonas within w_info_recepcion_resumida
event destroy ( )
integer x = 741
integer y = 640
integer taborder = 30
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_recepcion_resumida
event destroy ( )
integer x = 1358
integer y = 436
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

