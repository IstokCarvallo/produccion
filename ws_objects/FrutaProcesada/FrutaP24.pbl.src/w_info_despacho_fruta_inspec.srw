$PBExportHeader$w_info_despacho_fruta_inspec.srw
forward
global type w_info_despacho_fruta_inspec from w_para_informes
end type
type rb_fruta_inspec from radiobutton within w_info_despacho_fruta_inspec
end type
type rb_fruta_inspec_pl from radiobutton within w_info_despacho_fruta_inspec
end type
type st_4 from statictext within w_info_despacho_fruta_inspec
end type
type st_1 from statictext within w_info_despacho_fruta_inspec
end type
type dw_2 from datawindow within w_info_despacho_fruta_inspec
end type
type st_6 from statictext within w_info_despacho_fruta_inspec
end type
type dw_1 from datawindow within w_info_despacho_fruta_inspec
end type
type em_fecha from editmask within w_info_despacho_fruta_inspec
end type
type st_3 from statictext within w_info_despacho_fruta_inspec
end type
type st_2 from statictext within w_info_despacho_fruta_inspec
end type
type em_planilla from editmask within w_info_despacho_fruta_inspec
end type
type em_fecha_des from editmask within w_info_despacho_fruta_inspec
end type
type cbx_var from checkbox within w_info_despacho_fruta_inspec
end type
type cbx_prod1 from checkbox within w_info_despacho_fruta_inspec
end type
type cbx_prod2 from checkbox within w_info_despacho_fruta_inspec
end type
type st_7 from statictext within w_info_despacho_fruta_inspec
end type
type cbx_clie from checkbox within w_info_despacho_fruta_inspec
end type
type st_8 from statictext within w_info_despacho_fruta_inspec
end type
type gb_3 from groupbox within w_info_despacho_fruta_inspec
end type
type st_5 from statictext within w_info_despacho_fruta_inspec
end type
type rb_1 from radiobutton within w_info_despacho_fruta_inspec
end type
type rb_2 from radiobutton within w_info_despacho_fruta_inspec
end type
type gb_4 from groupbox within w_info_despacho_fruta_inspec
end type
type st_9 from statictext within w_info_despacho_fruta_inspec
end type
type rb_3 from radiobutton within w_info_despacho_fruta_inspec
end type
type st_13 from statictext within w_info_despacho_fruta_inspec
end type
type em_nomdespachador from editmask within w_info_despacho_fruta_inspec
end type
type st_10 from statictext within w_info_despacho_fruta_inspec
end type
type cbx_packing from checkbox within w_info_despacho_fruta_inspec
end type
type cbx_formatosag from checkbox within w_info_despacho_fruta_inspec
end type
type cbx_prdrot from checkbox within w_info_despacho_fruta_inspec
end type
type cbx_calrot from checkbox within w_info_despacho_fruta_inspec
end type
type st_11 from statictext within w_info_despacho_fruta_inspec
end type
type ddlb_1 from dropdownlistbox within w_info_despacho_fruta_inspec
end type
type rb_4 from radiobutton within w_info_despacho_fruta_inspec
end type
type st_12 from statictext within w_info_despacho_fruta_inspec
end type
type cbx_horizontal from checkbox within w_info_despacho_fruta_inspec
end type
type st_14 from statictext within w_info_despacho_fruta_inspec
end type
type rb_inspeccionado from radiobutton within w_info_despacho_fruta_inspec
end type
type rb_tratado from radiobutton within w_info_despacho_fruta_inspec
end type
type rb_tratainpe from radiobutton within w_info_despacho_fruta_inspec
end type
type st_22 from statictext within w_info_despacho_fruta_inspec
end type
type mle_condicion from multilineedit within w_info_despacho_fruta_inspec
end type
type st_16 from statictext within w_info_despacho_fruta_inspec
end type
type st_17 from statictext within w_info_despacho_fruta_inspec
end type
type em_cansellos from editmask within w_info_despacho_fruta_inspec
end type
type em_numsellos from editmask within w_info_despacho_fruta_inspec
end type
type st_18 from statictext within w_info_despacho_fruta_inspec
end type
type em_ubisellos from editmask within w_info_despacho_fruta_inspec
end type
type dw_3 from datawindow within w_info_despacho_fruta_inspec
end type
end forward

global type w_info_despacho_fruta_inspec from w_para_informes
integer x = 14
integer y = 32
integer width = 3250
integer height = 2588
string title = "Despacho de Fruta Inspeccionada S.A.G."
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
rb_fruta_inspec rb_fruta_inspec
rb_fruta_inspec_pl rb_fruta_inspec_pl
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_1 dw_1
em_fecha em_fecha
st_3 st_3
st_2 st_2
em_planilla em_planilla
em_fecha_des em_fecha_des
cbx_var cbx_var
cbx_prod1 cbx_prod1
cbx_prod2 cbx_prod2
st_7 st_7
cbx_clie cbx_clie
st_8 st_8
gb_3 gb_3
st_5 st_5
rb_1 rb_1
rb_2 rb_2
gb_4 gb_4
st_9 st_9
rb_3 rb_3
st_13 st_13
em_nomdespachador em_nomdespachador
st_10 st_10
cbx_packing cbx_packing
cbx_formatosag cbx_formatosag
cbx_prdrot cbx_prdrot
cbx_calrot cbx_calrot
st_11 st_11
ddlb_1 ddlb_1
rb_4 rb_4
st_12 st_12
cbx_horizontal cbx_horizontal
st_14 st_14
rb_inspeccionado rb_inspeccionado
rb_tratado rb_tratado
rb_tratainpe rb_tratainpe
st_22 st_22
mle_condicion mle_condicion
st_16 st_16
st_17 st_17
em_cansellos em_cansellos
em_numsellos em_numsellos
st_18 st_18
em_ubisellos em_ubisellos
dw_3 dw_3
end type
global w_info_despacho_fruta_inspec w_info_despacho_fruta_inspec

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, ii_var, ii_cli
String	is_report, is_tipoplanilla

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta


end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
end prototypes

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta, li_cansel, li_tecnic
Date		ld_fecha
String	ls_numsel, ls_ubisel, ls_tratam, ls_contraparte
Long		ll_fila, ll_fila1

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

If al_planilla <> 0 OR li_planta = 0 Then

	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND   defe_nturno =  :is_tipoplanilla
		AND	defe_plasag	=	:al_planilla;
				
	If sqlca.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		Return False
	ElseIf sqlca.SQLCode = 100 Then
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_planilla.SetFocus()
		Return False
	Else

		SELECT  isnull(defe_cansel,0), isnull(defe_numsel,''), isnull(defe_ubisel,''), defe_tratam, defe_tecnic
			INTO	:li_cansel,  :ls_numsel,  :ls_ubisel,  :ls_tratam,  :li_tecnic
			FROM	dbo.DESPAFRIGOEN 
			WHERE	plde_codigo =	:li_planta
			AND	clie_codigo	=	:li_codexp
			AND	defe_plasag	=	:al_planilla
			AND   defe_nturno =  :is_tipoplanilla
			AND   defe_fecdes =  :ld_fecha;				
		
		SELECT RTrim(tecn_nombre)+' '+RTrim(tecn_apepat)+' '+RTrim(tecn_apemat)
		   INTO  :ls_contraparte
		   FROM dbo.cargostecnicos
			WHERE tecn_codigo = :li_tecnic
			AND   plde_codigo = :li_planta;		
	
		em_fecha_des.text		= String(ld_fecha)
		em_fecha.text		= String(ld_fecha)
		pb_acepta.Enabled	= True
				
		em_nomdespachador.Text	=	ls_contraparte
			
		ll_fila = dw_3.Retrieve(li_planta,al_planilla)
			
		If ll_fila > 0 Then
			FOR ll_fila1 = 1 TO dw_3.RowCount()
				ls_numsel = ls_numsel+''+dw_3.Object.sell_numero[ll_fila1]+'-'
				ls_ubisel = ls_ubisel+''+dw_3.Object.sell_nombre[ll_fila1]+'-'
			NEXT
		End If	
		
		em_cansellos.Text = String(ll_fila)
		
		ls_numsel = Mid(ls_numsel, 1,len(ls_numsel) -1 )
		ls_ubisel = Mid(ls_ubisel, 1,len(ls_ubisel) -1 )
		
		em_numsellos.Text			=	ls_numsel
		em_ubisellos.Text			=	ls_ubisel
		
		Return True
	End If
Else
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", Exclamation!, Ok!)
	Return False
End If
end function

on w_info_despacho_fruta_inspec.create
int iCurrent
call super::create
this.rb_fruta_inspec=create rb_fruta_inspec
this.rb_fruta_inspec_pl=create rb_fruta_inspec_pl
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.em_fecha=create em_fecha
this.st_3=create st_3
this.st_2=create st_2
this.em_planilla=create em_planilla
this.em_fecha_des=create em_fecha_des
this.cbx_var=create cbx_var
this.cbx_prod1=create cbx_prod1
this.cbx_prod2=create cbx_prod2
this.st_7=create st_7
this.cbx_clie=create cbx_clie
this.st_8=create st_8
this.gb_3=create gb_3
this.st_5=create st_5
this.rb_1=create rb_1
this.rb_2=create rb_2
this.gb_4=create gb_4
this.st_9=create st_9
this.rb_3=create rb_3
this.st_13=create st_13
this.em_nomdespachador=create em_nomdespachador
this.st_10=create st_10
this.cbx_packing=create cbx_packing
this.cbx_formatosag=create cbx_formatosag
this.cbx_prdrot=create cbx_prdrot
this.cbx_calrot=create cbx_calrot
this.st_11=create st_11
this.ddlb_1=create ddlb_1
this.rb_4=create rb_4
this.st_12=create st_12
this.cbx_horizontal=create cbx_horizontal
this.st_14=create st_14
this.rb_inspeccionado=create rb_inspeccionado
this.rb_tratado=create rb_tratado
this.rb_tratainpe=create rb_tratainpe
this.st_22=create st_22
this.mle_condicion=create mle_condicion
this.st_16=create st_16
this.st_17=create st_17
this.em_cansellos=create em_cansellos
this.em_numsellos=create em_numsellos
this.st_18=create st_18
this.em_ubisellos=create em_ubisellos
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_fruta_inspec
this.Control[iCurrent+2]=this.rb_fruta_inspec_pl
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_1
this.Control[iCurrent+8]=this.em_fecha
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.em_planilla
this.Control[iCurrent+12]=this.em_fecha_des
this.Control[iCurrent+13]=this.cbx_var
this.Control[iCurrent+14]=this.cbx_prod1
this.Control[iCurrent+15]=this.cbx_prod2
this.Control[iCurrent+16]=this.st_7
this.Control[iCurrent+17]=this.cbx_clie
this.Control[iCurrent+18]=this.st_8
this.Control[iCurrent+19]=this.gb_3
this.Control[iCurrent+20]=this.st_5
this.Control[iCurrent+21]=this.rb_1
this.Control[iCurrent+22]=this.rb_2
this.Control[iCurrent+23]=this.gb_4
this.Control[iCurrent+24]=this.st_9
this.Control[iCurrent+25]=this.rb_3
this.Control[iCurrent+26]=this.st_13
this.Control[iCurrent+27]=this.em_nomdespachador
this.Control[iCurrent+28]=this.st_10
this.Control[iCurrent+29]=this.cbx_packing
this.Control[iCurrent+30]=this.cbx_formatosag
this.Control[iCurrent+31]=this.cbx_prdrot
this.Control[iCurrent+32]=this.cbx_calrot
this.Control[iCurrent+33]=this.st_11
this.Control[iCurrent+34]=this.ddlb_1
this.Control[iCurrent+35]=this.rb_4
this.Control[iCurrent+36]=this.st_12
this.Control[iCurrent+37]=this.cbx_horizontal
this.Control[iCurrent+38]=this.st_14
this.Control[iCurrent+39]=this.rb_inspeccionado
this.Control[iCurrent+40]=this.rb_tratado
this.Control[iCurrent+41]=this.rb_tratainpe
this.Control[iCurrent+42]=this.st_22
this.Control[iCurrent+43]=this.mle_condicion
this.Control[iCurrent+44]=this.st_16
this.Control[iCurrent+45]=this.st_17
this.Control[iCurrent+46]=this.em_cansellos
this.Control[iCurrent+47]=this.em_numsellos
this.Control[iCurrent+48]=this.st_18
this.Control[iCurrent+49]=this.em_ubisellos
this.Control[iCurrent+50]=this.dw_3
end on

on w_info_despacho_fruta_inspec.destroy
call super::destroy
destroy(this.rb_fruta_inspec)
destroy(this.rb_fruta_inspec_pl)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.em_fecha)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.em_planilla)
destroy(this.em_fecha_des)
destroy(this.cbx_var)
destroy(this.cbx_prod1)
destroy(this.cbx_prod2)
destroy(this.st_7)
destroy(this.cbx_clie)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.gb_4)
destroy(this.st_9)
destroy(this.rb_3)
destroy(this.st_13)
destroy(this.em_nomdespachador)
destroy(this.st_10)
destroy(this.cbx_packing)
destroy(this.cbx_formatosag)
destroy(this.cbx_prdrot)
destroy(this.cbx_calrot)
destroy(this.st_11)
destroy(this.ddlb_1)
destroy(this.rb_4)
destroy(this.st_12)
destroy(this.cbx_horizontal)
destroy(this.st_14)
destroy(this.rb_inspeccionado)
destroy(this.rb_tratado)
destroy(this.rb_tratainpe)
destroy(this.st_22)
destroy(this.mle_condicion)
destroy(this.st_16)
destroy(this.st_17)
destroy(this.em_cansellos)
destroy(this.em_numsellos)
destroy(this.st_18)
destroy(this.em_ubisellos)
destroy(this.dw_3)
end on

event open;x=0
y=0
dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"clie_codigo", gi_codexport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1,"plde_codigo", gi_codplanta)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

rb_fruta_inspec.Checked	=	True

em_fecha.text	=	String(Today())

ddlb_1.SelectItem(Integer(1))

is_tipoplanilla = '1'

//IF Upper(gstr_us.Nombre)  = 'GMADARIAGA' OR Upper(gstr_us.Nombre)  = 'PVALDES' OR &
//	Upper(gstr_us.Nombre)  = 'ACORTES' OR Upper(gstr_us.Nombre)  = 'JCORTES' OR  Upper(gstr_us.Nombre)  = 'JCORTES' THEN
//	cbx_horizontal.Checked 	= True
//	cbx_horizontal.Enabled 	= True
//ELSE
//	cbx_horizontal.Checked 	= False
//	cbx_horizontal.Enabled 	= False
//END IF	

IF gi_vari_rotulada = 1 THEN
	cbx_var.Checked	= True
	cbx_var.Enabled	= False
ELSE
	cbx_var.Checked	= False
	cbx_var.Enabled	= True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_prdrot.Checked	= True
	cbx_prdrot.Enabled	= False
ELSE
	cbx_prdrot.Checked	= False
	cbx_prdrot.Enabled	= True
END IF	

IF gi_cali_rotulado = 1 THEN
	cbx_calrot.Checked	= True
	cbx_calrot.Enabled	= False
ELSE
	cbx_calrot.Checked	= False
	cbx_calrot.Enabled	= True
END IF	

ii_var	= gi_vari_rotulada

dw_3.SetTransObject(sqlca)






end event

type pb_excel from w_para_informes`pb_excel within w_info_despacho_fruta_inspec
integer x = 2816
integer y = 1244
end type

type st_computador from w_para_informes`st_computador within w_info_despacho_fruta_inspec
end type

type st_usuario from w_para_informes`st_usuario within w_info_despacho_fruta_inspec
end type

type st_temporada from w_para_informes`st_temporada within w_info_despacho_fruta_inspec
end type

type p_logo from w_para_informes`p_logo within w_info_despacho_fruta_inspec
integer y = 24
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_despacho_fruta_inspec
integer width = 2441
string text = "Despacho de Fruta Inspeccionada S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despacho_fruta_inspec
integer x = 2875
integer y = 1708
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_planta, li_cliente, li_informe, li_prdrot, li_calrot
Long		ll_planilla_sag
String	ls_tipoins

IF rb_1.Checked THEN
	istr_info.titulo	= 'DESPACHO DE FRUTA INSPECCIONADA ENTRE PLANTAS ADSCRITAS'
ELSE
	istr_info.titulo	= 'DESPACHO DE FRUTA PARA SER FUMIGADA EN U.S.A.'
END IF

li_cliente			=	Integer(istr_mant.argumento[1])
li_planta			=	Integer(istr_mant.argumento[2])
ll_planilla_sag	=	Long(em_planilla.Text)

OpenWithParm(vinf,istr_info)

IF rb_fruta_inspec.Checked	THEN
	IF cbx_formatosag.Checked THEN
		vinf.dw_1.DataObject = "dw_info_desp_frutainspec_unisag" //
		li_informe	=	1
	ELSE
		IF cbx_prod1.checked THEN
			IF cbx_packing.Checked THEN
				vinf.dw_1.DataObject = "dw_info_desp_frutainspec_variproducpack"
				li_informe	=	0
			ELSE
				vinf.dw_1.DataObject = "dw_info_desp_frutainspec_variporproduc" //
				li_informe	=	0				
			END IF
		ELSE
			vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspec_variedad"
			li_informe	=	0			
		END IF
	END IF
END IF

IF cbx_horizontal.Checked THEN
	IF rb_fruta_inspec_pl.Checked THEN
		IF cbx_formatosag.Checked THEN
			vinf.dw_1.DataObject = "dw_info_despacho_fruta_unisag_destino" //
			li_informe	=	2		
		ELSE
		
			IF cbx_prod2.checked THEN
				vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspecporproduc_destino"  //
				li_informe	=	0			
			ELSE
				vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspeccionada_destino"
				li_informe	=	0
			END IF
		END IF
	END IF
ELSE
	IF rb_fruta_inspec_pl.Checked THEN
		IF cbx_formatosag.Checked THEN
			vinf.dw_1.DataObject = "dw_info_despacho_fruta_unisag" //
			li_informe	=	2		
		ELSE
		
			IF cbx_prod2.checked THEN
				vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspecporproduc"  //
				li_informe	=	0			
			ELSE
				vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspeccionada"
				li_informe	=	0
			END IF
		END IF
	END IF
END IF
IF cbx_prdrot.Checked THEN
	li_prdrot 	=	1
ELSE
	li_prdrot	=	0
END IF

IF cbx_calrot.Checked THEN
	li_calrot 	=	1
ELSE
	li_calrot	=	0
END IF

ls_tipoIns = String(mle_condicion.Text)

vinf.dw_1.SetTransObject(sqlca)

//IF rb_inspeccionado.Checked THEN
//	ls_tipoIns = 'INSPECCIONADO'
//ELSEIF rb_tratado.checked THEN
//	ls_tipoIns = 'TRATADO'
//ELSEIF	rb_tratainpe.Checked THEN
//	ls_tipoIns = 'TRATADO E INSPECCIONADO'
//END IF	

li_fila = vinf.dw_1.Retrieve(li_cliente,li_planta,ll_planilla_sag,ii_var,ii_cli,&
			li_informe,li_prdrot,li_calrot,is_tipoplanilla,ls_tipoins)
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF rb_1.Checked THEN
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_1.text + "'")	
   ELSEIF rb_2.Checked THEN
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_2.text + "'")
   ELSEIF rb_4.Checked THEN
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_4.text + "'")
   ELSE
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_3.text + "'")		
   END IF
	vinf.dw_1.Modify("planilla.text = '" + em_planilla.text + "'")
	vinf.dw_1.Modify("fechaemi.text = '" + em_fecha.text + "'")
	vinf.dw_1.Modify("t_defe_fecdes.text = '" + em_fecha.text + "'")	
	vinf.dw_1.Modify("t_nomdespachador.text = '" + em_nomdespachador.text + "'")	
	
	vinf.dw_1.Modify("t_cansellos.text = '" + em_cansellos.text + "'")
	vinf.dw_1.Modify("t_ubisellos.text = '" + em_ubisellos.text + "'")
	vinf.dw_1.Modify("t_numsellos.text = '" + em_numsellos.text + "'")
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_despacho_fruta_inspec
integer x = 2866
integer y = 1988
integer taborder = 90
end type

type rb_fruta_inspec from radiobutton within w_info_despacho_fruta_inspec
integer x = 343
integer y = 584
integer width = 704
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
string text = "Fruta Inspeccionada"
boolean checked = true
end type

event clicked;//rb_fruta_inspec.Checked	=	True
IF rb_fruta_inspec.Checked THEN
	cbx_prod1.Enabled = TRUE
	cbx_prod2.Enabled = FALSE
	cbx_prod2.Checked = FALSE
	rb_3.Checked = True
ELSE
	cbx_prod1.Enabled = FALSE
END IF
	
end event

type rb_fruta_inspec_pl from radiobutton within w_info_despacho_fruta_inspec
integer x = 343
integer y = 648
integer width = 1385
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
string text = "Fruta Inspeccionada entre Plantas Adscritas"
end type

event clicked;//rb_fruta_inspec_pl.Checked	=	True
IF rb_fruta_inspec_pl.Checked THEN
	cbx_prod2.Enabled = TRUE
	cbx_prod1.Enabled = FALSE
	cbx_prod1.Checked = FALSE
	rb_1.Checked = True
ELSE
	cbx_prod2.Enabled = FALSE
END IF
end event

type st_4 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 1352
integer width = 2441
integer height = 732
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_despacho_fruta_inspec
integer x = 343
integer y = 1564
integer width = 448
integer height = 84
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

type dw_2 from datawindow within w_info_despacho_fruta_inspec
integer x = 859
integer y = 1464
integer width = 1161
integer height = 92
integer taborder = 40
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(data)
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_1.Object.plde_codigo[1])
dw_1.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))




	
end event

type st_6 from statictext within w_info_despacho_fruta_inspec
integer x = 343
integer y = 1472
integer width = 448
integer height = 84
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

type dw_1 from datawindow within w_info_despacho_fruta_inspec
integer x = 859
integer y = 1560
integer width = 983
integer height = 92
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=String(data)
end event

type em_fecha from editmask within w_info_despacho_fruta_inspec
integer x = 859
integer y = 1364
integer width = 421
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_3 from statictext within w_info_despacho_fruta_inspec
integer x = 343
integer y = 1380
integer width = 448
integer height = 84
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
string text = "Fecha Emisión"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_despacho_fruta_inspec
integer x = 343
integer y = 1656
integer width = 448
integer height = 84
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
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type em_planilla from editmask within w_info_despacho_fruta_inspec
event getfocus pbm_ensetfocus
integer x = 859
integer y = 1656
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

event modified;IF ExistePlanilla(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type em_fecha_des from editmask within w_info_despacho_fruta_inspec
integer x = 1477
integer y = 1656
integer width = 421
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type cbx_var from checkbox within w_info_despacho_fruta_inspec
integer x = 471
integer y = 1264
integer width = 539
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
string text = "Variedad Rot."
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type cbx_prod1 from checkbox within w_info_despacho_fruta_inspec
integer x = 2112
integer y = 580
integer width = 503
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
string text = "Por Productor"
boolean checked = true
end type

type cbx_prod2 from checkbox within w_info_despacho_fruta_inspec
integer x = 2112
integer y = 644
integer width = 503
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
boolean enabled = false
string text = " Por Productor"
boolean checked = true
end type

type st_7 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 1188
integer width = 1056
integer height = 164
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_clie from checkbox within w_info_despacho_fruta_inspec
integer x = 1550
integer y = 1264
integer width = 480
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
string text = "Cliente Rot."
end type

event clicked;if this.checked = true then
	ii_cli = 1
else
	ii_cli = 0
end if	
end event

type st_8 from statictext within w_info_despacho_fruta_inspec
integer x = 1312
integer y = 1188
integer width = 1381
integer height = 164
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_despacho_fruta_inspec
integer x = 293
integer y = 520
integer width = 2354
integer height = 276
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Despacho"
end type

type st_5 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 512
integer width = 2441
integer height = 308
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

type rb_1 from radiobutton within w_info_despacho_fruta_inspec
integer x = 343
integer y = 888
integer width = 2121
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "DESPACHO DE FRUTA INSPECCIONADA ENTRE PLANTAS ADSCRITAS"
end type

event clicked;////rb_fruta_inspec.Checked	=	True
//IF rb_fruta_inspec.Checked THEN
//	cbx_prod1.Enabled = TRUE
//	cbx_prod2.Enabled = FALSE
//	cbx_prod2.Checked = FALSE
//ELSE
//	cbx_prod1.Enabled = FALSE
//END IF

cbx_packing.Enabled = FALSE
cbx_packing.Checked = FALSE

IF rb_fruta_inspec.Checked THEN
	MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)
END IF						
end event

type rb_2 from radiobutton within w_info_despacho_fruta_inspec
integer x = 343
integer y = 948
integer width = 1669
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "DESPACHO DE FRUTA PARA SER FUMIGADA EN U.S.A."
end type

event clicked;////rb_fruta_inspec_pl.Checked	=	True
//IF rb_fruta_inspec_pl.Checked THEN
//	cbx_prod2.Enabled = TRUE
//	cbx_prod1.Enabled = FALSE
//	cbx_prod1.Checked = FALSE
//ELSE
//	cbx_prod2.Enabled = FALSE
//END IF

IF rb_fruta_inspec.Checked THEN
	cbx_packing.Enabled = TRUE
	cbx_packing.Checked = TRUE
ELSE
	cbx_packing.Enabled = FALSE
	cbx_packing.Checked = FALSE
END IF

//Duda con esta alerta
MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)

end event

type gb_4 from groupbox within w_info_despacho_fruta_inspec
integer x = 293
integer y = 828
integer width = 2354
integer height = 324
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Titulo"
end type

type st_9 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 820
integer width = 2441
integer height = 368
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

type rb_3 from radiobutton within w_info_despacho_fruta_inspec
integer x = 343
integer y = 1008
integer width = 1778
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
string text = "DESPACHO DE FRUTA INSPECCIONADA"
boolean checked = true
end type

event clicked;////rb_fruta_inspec.Checked	=	True
//IF rb_fruta_inspec.Checked THEN
//	cbx_prod1.Enabled = TRUE
//	cbx_prod2.Enabled = FALSE
//	cbx_prod2.Checked = FALSE
//ELSE
//	cbx_prod1.Enabled = FALSE
//END IF

cbx_packing.Enabled = FALSE
cbx_packing.Checked = FALSE

IF rb_fruta_inspec_pl.Checked THEN
	MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)
END IF			

	
end event

type st_13 from statictext within w_info_despacho_fruta_inspec
integer x = 343
integer y = 2092
integer width = 658
integer height = 84
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
string text = "Nombre Despachador "
boolean focusrectangle = false
end type

type em_nomdespachador from editmask within w_info_despacho_fruta_inspec
event getfocus pbm_ensetfocus
integer x = 1015
integer y = 2092
integer width = 1285
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_10 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 2080
integer width = 2441
integer height = 124
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_despacho_fruta_inspec
integer x = 2126
integer y = 976
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
boolean enabled = false
string text = "Por Packing"
end type

type cbx_formatosag from checkbox within w_info_despacho_fruta_inspec
integer x = 1056
integer y = 424
integer width = 709
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
string text = "Formato Único S.A.G."
boolean checked = true
end type

type cbx_prdrot from checkbox within w_info_despacho_fruta_inspec
integer x = 1550
integer y = 1196
integer width = 850
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
string text = "Productor Rotulado"
boolean checked = true
end type

type cbx_calrot from checkbox within w_info_despacho_fruta_inspec
integer x = 471
integer y = 1196
integer width = 795
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
string text = "Calibre Rotulado Repa  "
end type

type st_11 from statictext within w_info_despacho_fruta_inspec
integer x = 338
integer y = 1780
integer width = 448
integer height = 84
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
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type ddlb_1 from dropdownlistbox within w_info_despacho_fruta_inspec
integer x = 859
integer y = 1760
integer width = 1403
integer height = 400
integer taborder = 100
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

type rb_4 from radiobutton within w_info_despacho_fruta_inspec
integer x = 343
integer y = 1084
integer width = 1746
integer height = 48
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "DESPACHO DE FRUTA PARA SER FUMIGADA EN MEXICO"
end type

event clicked;////rb_fruta_inspec_pl.Checked	=	True
//IF rb_fruta_inspec_pl.Checked THEN
//	cbx_prod2.Enabled = TRUE
//	cbx_prod1.Enabled = FALSE
//	cbx_prod1.Checked = FALSE
//ELSE
//	cbx_prod2.Enabled = FALSE
//END IF

IF rb_fruta_inspec.Checked THEN
	cbx_packing.Enabled = TRUE
	cbx_packing.Checked = TRUE
ELSE
	cbx_packing.Enabled = FALSE
	cbx_packing.Checked = FALSE
END IF


MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)
	
end event

type st_12 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 404
integer width = 2441
integer height = 108
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

type cbx_horizontal from checkbox within w_info_despacho_fruta_inspec
integer x = 1056
integer y = 712
integer width = 1088
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
string text = "Formato con Destino Horizonal"
boolean checked = true
end type

type st_14 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 2204
integer width = 2441
integer height = 132
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_inspeccionado from radiobutton within w_info_despacho_fruta_inspec
boolean visible = false
integer x = 507
integer y = 2360
integer width = 613
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "INSPECCIONADO"
boolean checked = true
end type

type rb_tratado from radiobutton within w_info_despacho_fruta_inspec
boolean visible = false
integer x = 1266
integer y = 2360
integer width = 462
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "TRATADO"
end type

type rb_tratainpe from radiobutton within w_info_despacho_fruta_inspec
boolean visible = false
integer x = 1810
integer y = 2360
integer width = 983
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "TRATADO E INSPECCIONADO"
end type

type st_22 from statictext within w_info_despacho_fruta_inspec
integer x = 343
integer y = 2232
integer width = 663
integer height = 84
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
string text = "Condición"
boolean focusrectangle = false
end type

type mle_condicion from multilineedit within w_info_despacho_fruta_inspec
integer x = 1015
integer y = 2224
integer width = 736
integer height = 88
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "FRESCO"
integer limit = 20
borderstyle borderstyle = stylelowered!
end type

type st_16 from statictext within w_info_despacho_fruta_inspec
integer x = 251
integer y = 1356
integer width = 2441
integer height = 708
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

type st_17 from statictext within w_info_despacho_fruta_inspec
integer x = 338
integer y = 1880
integer width = 613
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
string text = "Números de Sellos"
boolean focusrectangle = false
end type

type em_cansellos from editmask within w_info_despacho_fruta_inspec
event getfocus pbm_ensetfocus
integer x = 992
integer y = 1968
integer width = 160
integer height = 84
integer taborder = 150
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

type em_numsellos from editmask within w_info_despacho_fruta_inspec
event getfocus pbm_ensetfocus
integer x = 992
integer y = 1876
integer width = 1367
integer height = 84
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_18 from statictext within w_info_despacho_fruta_inspec
integer x = 338
integer y = 1976
integer width = 635
integer height = 84
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
string text = "Cantidad - Ubicación "
boolean focusrectangle = false
end type

type em_ubisellos from editmask within w_info_despacho_fruta_inspec
event getfocus pbm_ensetfocus
integer x = 1157
integer y = 1968
integer width = 1202
integer height = 84
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type dw_3 from datawindow within w_info_despacho_fruta_inspec
boolean visible = false
integer x = 3241
integer y = 1400
integer width = 686
integer height = 400
integer taborder = 160
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dw_ubicacionessellossag"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

