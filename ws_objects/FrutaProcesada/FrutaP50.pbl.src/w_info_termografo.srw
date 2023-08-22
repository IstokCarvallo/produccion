$PBExportHeader$w_info_termografo.srw
forward
global type w_info_termografo from w_para_informes
end type
type st_4 from statictext within w_info_termografo
end type
type st_1 from statictext within w_info_termografo
end type
type dw_2 from datawindow within w_info_termografo
end type
type st_6 from statictext within w_info_termografo
end type
type dw_1 from datawindow within w_info_termografo
end type
type st_2 from statictext within w_info_termografo
end type
type em_planilla from editmask within w_info_termografo
end type
type em_fecha_des from editmask within w_info_termografo
end type
type st_9 from statictext within w_info_termografo
end type
type st_10 from statictext within w_info_termografo
end type
type st_11 from statictext within w_info_termografo
end type
type st_12 from statictext within w_info_termografo
end type
type st_13 from statictext within w_info_termografo
end type
type em_ubicacion from editmask within w_info_termografo
end type
type em_de from editmask within w_info_termografo
end type
type em_observaciones from editmask within w_info_termografo
end type
type em_a from editmask within w_info_termografo
end type
type gb_4 from groupbox within w_info_termografo
end type
type cbx_1 from checkbox within w_info_termografo
end type
type cbx_2 from checkbox within w_info_termografo
end type
type cbx_3 from checkbox within w_info_termografo
end type
type cbx_4 from checkbox within w_info_termografo
end type
type cbx_5 from checkbox within w_info_termografo
end type
type cbx_6 from checkbox within w_info_termografo
end type
type gb_3 from groupbox within w_info_termografo
end type
type st_18 from statictext within w_info_termografo
end type
type st_3 from statictext within w_info_termografo
end type
type cbx_calidad from checkbox within w_info_termografo
end type
type cbx_variedad from checkbox within w_info_termografo
end type
type cbx_productor from checkbox within w_info_termografo
end type
type cbx_packing from checkbox within w_info_termografo
end type
type cbx_fecha from checkbox within w_info_termografo
end type
type cbx_embalaje from checkbox within w_info_termografo
end type
type em_x from editmask within w_info_termografo
end type
end forward

global type w_info_termografo from w_para_informes
integer x = 14
integer y = 32
integer width = 3195
integer height = 2136
string title = "Informe Por Termógrafo"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_1 dw_1
st_2 st_2
em_planilla em_planilla
em_fecha_des em_fecha_des
st_9 st_9
st_10 st_10
st_11 st_11
st_12 st_12
st_13 st_13
em_ubicacion em_ubicacion
em_de em_de
em_observaciones em_observaciones
em_a em_a
gb_4 gb_4
cbx_1 cbx_1
cbx_2 cbx_2
cbx_3 cbx_3
cbx_4 cbx_4
cbx_5 cbx_5
cbx_6 cbx_6
gb_3 gb_3
st_18 st_18
st_3 st_3
cbx_calidad cbx_calidad
cbx_variedad cbx_variedad
cbx_productor cbx_productor
cbx_packing cbx_packing
cbx_fecha cbx_fecha
cbx_embalaje cbx_embalaje
em_x em_x
end type
global w_info_termografo w_info_termografo

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo , ii_var, ii_cli
String	is_report, is_nombre, is_rut, is_tipopla

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public subroutine rellena_campos (integer inicio, integer termino, string ls_texto[], string campo)
public function boolean buscanombre (integer ai_cliente)
public function integer buscatipodespacho (integer cliente, integer planta, long planilla, string tipopla)
public function string rescata_contenedor (integer cliente, integer planta, long planilla, string tipopla)
public subroutine rescata_origen (integer cliente, integer planta, long planilla, string tipopla, integer prodrot)
public function boolean buscaplanta (integer ai_planta)
end prototypes

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date		ld_fecha
Long 		ll_numero

li_codexp		=	Integer(istr_mant.argumento[1])


IF al_planilla <> 0 OR li_planta = 0 THEN

	SELECT Max(embq_fzarpe)
		INTO	:ld_fecha
		FROM	dbo.embarqueprod 
		WHERE	clie_codigo	=	:li_codexp
		AND	oper_codigo =  :al_planilla	;				
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla embarqueprod")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Operacion Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSE
		em_fecha_des.text		= String(ld_fecha)
		pb_acepta.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public subroutine rellena_campos (integer inicio, integer termino, string ls_texto[], string campo);/*=======================================================================
Escribe en la planilla dentro de las lineas de "Observaciones" Las Provincias
y las Comunas en forma Lineal
========================================================================*/
integer li_int1;
string ls_data;

For li_int1 = inicio to termino
	
	 ls_data= ls_data +" "+ls_texto[li_int1];
		
next	

vinf.dw_1.Modify(campo + ls_data + "'")
end subroutine

public function boolean buscanombre (integer ai_cliente);Integer	li_codexp, li_planta

SELECT clie_nombre,clie_nrorut
	INTO	:is_nombre,:is_rut
	FROM	dbo.clientesprod
	WHERE	clie_codigo	=	:ai_cliente;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ClientesProd")
	Return False
	
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No Existe Cliente.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
	Return False						
	END IF
	Return True
	
end function

public function integer buscatipodespacho (integer cliente, integer planta, long planilla, string tipopla);Integer	li_codexp, li_planta, li_tipo = 0
Date		ld_fecha

IF planilla <> 0 THEN

	SELECT max(defe_tiposa)
		INTO	:li_tipo
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:planta
		AND	clie_codigo	=	:cliente
		AND	defe_plasag	=	:planilla
		AND   defe_nturno	=	:tipopla;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		li_tipo = 0
		RETURN li_tipo
	ELSEIF sqlca.SQLCode = 100 THEN
		li_tipo = 0
		RETURN li_tipo
	ELSE
		RETURN li_tipo
	END IF
ELSE
	RETURN li_tipo
END IF
end function

public function string rescata_contenedor (integer cliente, integer planta, long planilla, string tipopla);String Contenedor;

SELECT defe_nrcont INTO :Contenedor FROM dbo.despafrigoen WHERE
	clie_codigo=:cliente AND 
	plde_codigo=:planta AND
	defe_plasag=:planilla  AND
	defe_nturno=:tipopla
GROUP BY defe_nrcont;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla despafrigoen")
	
END IF

RETURN Contenedor;
end function

public subroutine rescata_origen (integer cliente, integer planta, long planilla, string tipopla, integer prodrot);/*=======================================================
Se instancia un datastore para traer registros de la
dw_info_planilla_despacho_prov_comuna y rescatar el origen
========================================================*/
integer li_int1, li_filas, li_cont;
string ls_texto[99], ls_data,ls_data2,ls_contenedor;

datastore data_1;
data_1 = CREATE datastore;
data_1.dataobject="dw_info_planilla_despacho_prov_comuna"
data_1.settransobject(sqlca);

li_filas=data_1.retrieve(cliente,planta,planilla,tipopla,Prodrot);

li_cont=1;
ls_data=data_1.object.prod_provin[li_cont]	
ls_texto[li_cont]="Provincia:"+ls_data
//ls_texto[li_cont]=ls_data
li_cont=li_cont + 1;

/*==================================================
Recorre la Datastore data_1 para rescatar provincias
==================================================*/
FOR li_int1 = 2 to li_filas
	
	 ls_data=data_1.object.prod_provin[li_int1]	
	 ls_data2=data_1.object.prod_provin[li_int1 -1]
	 
	IF trim(ls_data) <> trim(ls_data2) then
		ls_texto[li_cont]=ls_data
		li_cont=li_cont+1;
	end if	
	
next

/*=================================================
Recorre la Datastore data_1 para rescatar comunas
==================================================*/
ls_texto[li_cont]="/Comuna:"
li_cont=li_cont+1;

ls_data=""
FOR li_int1 = 1 to li_filas
	
	 ls_data=data_1.object.prod_comuna[li_int1]	
	 ls_texto[li_cont]=ls_data 
	 li_cont=li_cont+1;
next


end subroutine

public function boolean buscaplanta (integer ai_planta);Integer	li_codexp, li_planta

SELECT plde_nombre
	INTO	:is_nombre
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:ai_planta;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla plantadesp")
	Return False
	
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No Existe Planta.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
	Return False						
	END IF
	Return True
	
end function

on w_info_termografo.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.st_2=create st_2
this.em_planilla=create em_planilla
this.em_fecha_des=create em_fecha_des
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.st_12=create st_12
this.st_13=create st_13
this.em_ubicacion=create em_ubicacion
this.em_de=create em_de
this.em_observaciones=create em_observaciones
this.em_a=create em_a
this.gb_4=create gb_4
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.cbx_4=create cbx_4
this.cbx_5=create cbx_5
this.cbx_6=create cbx_6
this.gb_3=create gb_3
this.st_18=create st_18
this.st_3=create st_3
this.cbx_calidad=create cbx_calidad
this.cbx_variedad=create cbx_variedad
this.cbx_productor=create cbx_productor
this.cbx_packing=create cbx_packing
this.cbx_fecha=create cbx_fecha
this.cbx_embalaje=create cbx_embalaje
this.em_x=create em_x
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.em_planilla
this.Control[iCurrent+8]=this.em_fecha_des
this.Control[iCurrent+9]=this.st_9
this.Control[iCurrent+10]=this.st_10
this.Control[iCurrent+11]=this.st_11
this.Control[iCurrent+12]=this.st_12
this.Control[iCurrent+13]=this.st_13
this.Control[iCurrent+14]=this.em_ubicacion
this.Control[iCurrent+15]=this.em_de
this.Control[iCurrent+16]=this.em_observaciones
this.Control[iCurrent+17]=this.em_a
this.Control[iCurrent+18]=this.gb_4
this.Control[iCurrent+19]=this.cbx_1
this.Control[iCurrent+20]=this.cbx_2
this.Control[iCurrent+21]=this.cbx_3
this.Control[iCurrent+22]=this.cbx_4
this.Control[iCurrent+23]=this.cbx_5
this.Control[iCurrent+24]=this.cbx_6
this.Control[iCurrent+25]=this.gb_3
this.Control[iCurrent+26]=this.st_18
this.Control[iCurrent+27]=this.st_3
this.Control[iCurrent+28]=this.cbx_calidad
this.Control[iCurrent+29]=this.cbx_variedad
this.Control[iCurrent+30]=this.cbx_productor
this.Control[iCurrent+31]=this.cbx_packing
this.Control[iCurrent+32]=this.cbx_fecha
this.Control[iCurrent+33]=this.cbx_embalaje
this.Control[iCurrent+34]=this.em_x
end on

on w_info_termografo.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.st_2)
destroy(this.em_planilla)
destroy(this.em_fecha_des)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_ubicacion)
destroy(this.em_de)
destroy(this.em_observaciones)
destroy(this.em_a)
destroy(this.gb_4)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.cbx_4)
destroy(this.cbx_5)
destroy(this.cbx_6)
destroy(this.gb_3)
destroy(this.st_18)
destroy(this.st_3)
destroy(this.cbx_calidad)
destroy(this.cbx_variedad)
destroy(this.cbx_productor)
destroy(this.cbx_packing)
destroy(this.cbx_fecha)
destroy(this.cbx_embalaje)
destroy(this.em_x)
end on

event open;call super::open;dw_2.GetChild("clie_codigo", idwc_cliente)
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

IF buscaplanta(gi_codplanta)THEN
	em_de.Text = is_nombre
END IF	

IF gi_vari_rotulada = 1 THEN
	cbx_variedad.Checked	= True
	cbx_variedad.Enabled	= False
ELSE
	cbx_variedad.Checked	= False
	cbx_variedad.Enabled	= True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_productor.Checked	= True
	cbx_productor.Enabled	= False
ELSE
	cbx_productor.Checked	= False
	cbx_productor.Enabled	= True
END IF	

IF gi_cali_rotulado = 1 THEN
	cbx_calidad.Checked	= True
	cbx_calidad.Enabled	= False
ELSE
	cbx_calidad.Checked	= False
	cbx_calidad.Enabled	= True
END IF	

IF gi_pack_rotulado = 1 THEN
	cbx_packing.Checked	= True
	cbx_packing.Enabled	= False
ELSE
	cbx_packing.Checked	= False
	cbx_packing.Enabled	= True
END IF	

em_a.Text = 'FERNANDO SAT'
end event

type pb_excel from w_para_informes`pb_excel within w_info_termografo
end type

type st_computador from w_para_informes`st_computador within w_info_termografo
end type

type st_usuario from w_para_informes`st_usuario within w_info_termografo
end type

type st_temporada from w_para_informes`st_temporada within w_info_termografo
end type

type p_logo from w_para_informes`p_logo within w_info_termografo
end type

type st_titulo from w_para_informes`st_titulo within w_info_termografo
integer width = 2331
string text = "Informe por Termógrafo"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_termografo
string tag = "Imprimir Reporte"
integer x = 2761
integer y = 1408
integer taborder = 140
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_planta, li_cliente, li_prdrot, li_calrot, li_varrot, li_packrot, li_embrot, li_fecrot
Long		ll_operacion
String	ls_glosa

li_prdrot =0
li_calrot =0 
li_varrot =0 
li_packrot=0 
li_embrot =0 
li_fecrot =0

istr_info.titulo	= 'PLANILLA DE DESPACHO FRUTA SAG'

li_cliente			=	Integer(istr_mant.argumento[1])
li_planta			=	Integer(istr_mant.argumento[2])
ll_operacion		=	Long(em_planilla.Text)

IF cbx_calidad.Checked THEN
	li_calrot = 1
END IF	

IF cbx_variedad.Checked THEN
	li_varrot = 1
END IF	

IF cbx_packing.Checked THEN
	li_packrot = 1
END IF

IF cbx_calidad.Checked THEN
	li_calrot = 1
END IF

IF cbx_embalaje.Checked THEN
	li_embrot = 1
END IF

IF cbx_fecha.Checked THEN
	li_fecrot = 1
END IF

IF cbx_productor.Checked THEN
	li_prdrot = 1
END IF

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_termografo"

vinf.dw_1.SetTransObject(SQLCA)

li_fila			=	vinf.dw_1.Retrieve(li_cliente,li_planta,ll_operacion,li_calrot,&
					li_varrot,li_packrot,li_fecrot,li_embrot,li_prdrot)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	IF cbx_1.Checked THEN
		vinf.dw_1.Modify("t_40.text = '" + em_x.text + "'")
	END IF	
	IF cbx_2.Checked THEN
		vinf.dw_1.Modify("t_41.text = '" + em_x.text + "'")
	END IF	
	IF cbx_3.Checked THEN
		vinf.dw_1.Modify("t_42.text = '" + em_x.text + "'")
	END IF	
	IF cbx_4.Checked THEN
		vinf.dw_1.Modify("t_43.text = '" + em_x.text + "'")
	END IF	
	IF cbx_5.Checked THEN
		vinf.dw_1.Modify("t_44.text = '" + em_x.text + "'")
	END IF	
	IF cbx_6.Checked THEN
		vinf.dw_1.Modify("t_45.text = '" + em_x.text + "'")
	END IF	
		
	
	vinf.dw_1.Modify("t_4.text = '" + em_a.text + "'")
	IF em_de.Text <> '' THEN 
		vinf.dw_1.Modify("t_47.text = '" + em_de.text + "'")
	END IF	
	
	vinf.dw_1.Modify("t_13.text = '" + em_ubicacion.text + "'")
	
	IF em_observaciones.Text <> '' THEN
		vinf.dw_1.Modify("t_46.text = '" + em_observaciones.text + "'")
	END IF	
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_termografo
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2766
integer y = 1688
integer taborder = 150
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_termografo
integer x = 247
integer y = 440
integer width = 2331
integer height = 336
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

type st_1 from statictext within w_info_termografo
integer x = 325
integer y = 576
integer width = 448
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

type dw_2 from datawindow within w_info_termografo
integer x = 1051
integer y = 456
integer width = 1161
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(data)
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_1.Object.plde_codigo[1])
dw_1.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))

IF BuscaNombre(Integer(Data)) THEN
//	em_exportador.Text = is_nombre
//	em_rut.Text = is_rut
END IF	
end event

type st_6 from statictext within w_info_termografo
integer x = 325
integer y = 480
integer width = 448
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

type dw_1 from datawindow within w_info_termografo
integer x = 1051
integer y = 556
integer width = 983
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=String(data)
end event

type st_2 from statictext within w_info_termografo
integer x = 325
integer y = 672
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
string text = "Operación"
boolean focusrectangle = false
end type

type em_planilla from editmask within w_info_termografo
event getfocus pbm_ensetfocus
integer x = 1051
integer y = 656
integer width = 443
integer height = 92
integer taborder = 40
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

type em_fecha_des from editmask within w_info_termografo
integer x = 1531
integer y = 656
integer width = 439
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_9 from statictext within w_info_termografo
integer x = 247
integer y = 776
integer width = 2331
integer height = 532
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

type st_10 from statictext within w_info_termografo
integer x = 325
integer y = 1012
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
string text = "Ubicación "
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_termografo
integer x = 325
integer y = 808
integer width = 763
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
string text = "A"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_termografo
integer x = 325
integer y = 904
integer width = 658
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
string text = "De"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_termografo
integer x = 325
integer y = 1108
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
string text = "Observaciones"
boolean focusrectangle = false
end type

type em_ubicacion from editmask within w_info_termografo
event getfocus pbm_ensetfocus
integer x = 1047
integer y = 992
integer width = 1477
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
string text = "TERCERA CAJA DE ARRIBA HACIA ABAJO"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type em_de from editmask within w_info_termografo
event getfocus pbm_ensetfocus
integer x = 1047
integer y = 892
integer width = 1477
integer height = 92
integer taborder = 90
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

type em_observaciones from editmask within w_info_termografo
event getfocus pbm_ensetfocus
integer x = 1047
integer y = 1092
integer width = 1477
integer height = 196
integer taborder = 120
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
boolean autoskip = true
string displaydata = "$"
end type

type em_a from editmask within w_info_termografo
event getfocus pbm_ensetfocus
integer x = 1047
integer y = 796
integer width = 1477
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
string text = "FERNANDO SAT"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type gb_4 from groupbox within w_info_termografo
integer x = 1472
integer y = 1316
integer width = 1088
integer height = 560
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Rotulados"
end type

type cbx_1 from checkbox within w_info_termografo
integer x = 338
integer y = 1384
integer width = 1033
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
string text = "Packing"
boolean lefttext = true
end type

type cbx_2 from checkbox within w_info_termografo
integer x = 338
integer y = 1464
integer width = 1033
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
string text = "Antes del Prefrío"
boolean lefttext = true
end type

type cbx_3 from checkbox within w_info_termografo
integer x = 338
integer y = 1544
integer width = 1033
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
string text = "Entre Prefrío y Mantención"
boolean lefttext = true
end type

type cbx_4 from checkbox within w_info_termografo
integer x = 338
integer y = 1624
integer width = 1033
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
string text = "Mantención (Más de 24 hr.)"
boolean lefttext = true
end type

type cbx_5 from checkbox within w_info_termografo
integer x = 338
integer y = 1704
integer width = 1033
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
string text = "Durante el Embarque"
boolean lefttext = true
end type

type cbx_6 from checkbox within w_info_termografo
integer x = 338
integer y = 1784
integer width = 1033
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
string text = "Puerto"
boolean lefttext = true
end type

type gb_3 from groupbox within w_info_termografo
integer x = 279
integer y = 1316
integer width = 1143
integer height = 560
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lugar"
end type

type st_18 from statictext within w_info_termografo
integer x = 247
integer y = 1308
integer width = 1193
integer height = 588
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

type st_3 from statictext within w_info_termografo
integer x = 1440
integer y = 1308
integer width = 1138
integer height = 588
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

type cbx_calidad from checkbox within w_info_termografo
integer x = 1495
integer y = 1384
integer width = 1024
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
string text = "Calidad"
boolean lefttext = true
end type

type cbx_variedad from checkbox within w_info_termografo
integer x = 1495
integer y = 1464
integer width = 1024
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
string text = "Variedad"
boolean lefttext = true
end type

type cbx_productor from checkbox within w_info_termografo
integer x = 1495
integer y = 1544
integer width = 1024
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
string text = "Productor"
boolean lefttext = true
end type

type cbx_packing from checkbox within w_info_termografo
integer x = 1495
integer y = 1624
integer width = 1024
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
string text = "Packing"
boolean lefttext = true
end type

type cbx_fecha from checkbox within w_info_termografo
integer x = 1495
integer y = 1704
integer width = 1024
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
string text = "Fecha Embalaje"
boolean lefttext = true
end type

type cbx_embalaje from checkbox within w_info_termografo
integer x = 1495
integer y = 1784
integer width = 1024
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
string text = "Embalaje"
boolean lefttext = true
end type

type em_x from editmask within w_info_termografo
boolean visible = false
integer x = 1778
integer y = 1932
integer width = 402
integer height = 112
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "X"
borderstyle borderstyle = stylelowered!
end type

