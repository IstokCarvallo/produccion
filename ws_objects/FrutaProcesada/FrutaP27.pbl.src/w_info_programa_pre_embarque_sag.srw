$PBExportHeader$w_info_programa_pre_embarque_sag.srw
forward
global type w_info_programa_pre_embarque_sag from w_para_informes
end type
type st_4 from statictext within w_info_programa_pre_embarque_sag
end type
type st_1 from statictext within w_info_programa_pre_embarque_sag
end type
type dw_2 from datawindow within w_info_programa_pre_embarque_sag
end type
type st_6 from statictext within w_info_programa_pre_embarque_sag
end type
type dw_1 from datawindow within w_info_programa_pre_embarque_sag
end type
type em_fecha from editmask within w_info_programa_pre_embarque_sag
end type
type st_3 from statictext within w_info_programa_pre_embarque_sag
end type
type st_2 from statictext within w_info_programa_pre_embarque_sag
end type
type em_planilla from editmask within w_info_programa_pre_embarque_sag
end type
type em_fecha_des from editmask within w_info_programa_pre_embarque_sag
end type
type st_5 from statictext within w_info_programa_pre_embarque_sag
end type
type ddlb_sitios from dropdownlistbox within w_info_programa_pre_embarque_sag
end type
type dw_3 from datawindow within w_info_programa_pre_embarque_sag
end type
end forward

global type w_info_programa_pre_embarque_sag from w_para_informes
integer x = 14
integer y = 32
integer width = 2670
integer height = 1428
string title = "Programa Pre-Embarque S.A.G."
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
em_fecha em_fecha
st_3 st_3
st_2 st_2
em_planilla em_planilla
em_fecha_des em_fecha_des
st_5 st_5
ddlb_sitios ddlb_sitios
dw_3 dw_3
end type
global w_info_programa_pre_embarque_sag w_info_programa_pre_embarque_sag

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo , ii_var, ii_cli
String	is_report

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, &
						idwc_pueremb, idwc_puertos, idwc_mercado, idwc_sitios
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public function integer buscatipodespacho (integer cliente, integer planta, long planilla)
public subroutine rescata_origen (integer cliente, integer planta, integer planilla)
public subroutine rellena_campos (integer inicio, integer termino, string ls_texto[], string campo)
public function string rescata_contenedor (integer cliente, integer planta, long planilla)
end prototypes

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date		ld_fecha
Long 		ll_numero,ll_sitio

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_planilla <> 0 OR li_planta = 0 THEN

	SELECT Max(defe_fecdes),Max(defe_numero),sire_codigo
		INTO	:ld_fecha,:ll_numero,:ll_sitio
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla
		GROUP BY sire_codigo;					
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSE
		em_fecha_des.text		= String(ld_fecha)
		pb_acepta.Enabled	= True
		dw_3.SetItem(1,'sire_codigo',ll_sitio)

		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function integer buscatipodespacho (integer cliente, integer planta, long planilla);Integer	li_codexp, li_planta, li_tipo = 0
Date		ld_fecha

IF planilla <> 0 THEN

	SELECT max(defe_tiposa)
		INTO	:li_tipo
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:planta
		AND	clie_codigo	=	:cliente
		AND	defe_plasag	=	:planilla;
				
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

public subroutine rescata_origen (integer cliente, integer planta, integer planilla);/*=======================================================
Se instancia un datastore para traer registros de la
dw_info_planilla_despacho_prov_comuna y rescatar el origen
========================================================*/
integer li_int1, li_filas, li_cont;
string ls_texto[25], ls_data,ls_data2,ls_contenedor;

datastore data_1;
data_1 = CREATE datastore;
data_1.dataobject="dw_info_planilla_despacho_prov_comuna"
data_1.settransobject(sqlca);

li_filas=data_1.retrieve(cliente,planta,planilla);

li_cont=1;
//ls_data=data_1.object.prod_provin[li_cont]	
//ls_texto[li_cont]=ls_data
//li_cont=li_cont + 1; 

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
ls_texto[li_cont]="/COMUNAS:"
li_cont=li_cont+1;

ls_data=""
FOR li_int1 = 1 to li_filas
	
	 ls_data=data_1.object.prod_comuna[li_int1]	
	 ls_texto[li_cont]=ls_data 
	 li_cont=li_cont+1;
next

/*=======================================
Agrega en informe el numero de contenedor 
solo para informe de productos agrícolas
=======================================*/
//IF rb_prod_agri.Checked=TRUE THEN 
//	
	ls_contenedor=rescata_contenedor(cliente,planta,planilla);
	IF NOT ISNULL(ls_contenedor) THEN
		ls_texto[li_cont]="/CONTENEDOR:" 
	 	li_cont=li_cont+1
		ls_texto[li_cont]=ls_contenedor; 
	 	li_cont=li_cont+1 
	END IF
//END IF	
//
/*=================================================
Rellena los objectos de texto de la planilla con los
datos de Origen almacenados en ls_texto[li_int1]
==================================================*/
//IF rb_fumig_usa.Checked=TRUE THEN
//	
//	rellena_campos(1,5,ls_texto,"t_campo1.text = '")
//	rellena_campos(6,10,ls_texto,"t_campo2.text = '")
//	rellena_campos(11,li_cont,ls_texto,"t_campo3.text = '")
//	
//ELSE
//	
//	rellena_campos(1,3,ls_texto,"t_campo1.text = '")
//	rellena_campos(4,8,ls_texto,"t_campo2.text = '")
//	rellena_campos(9,14,ls_texto,"t_campo3.text = '")
//	rellena_campos(15,li_cont,ls_texto,"t_campo4.text = '")
   	
//END IF
//
//
end subroutine

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

public function string rescata_contenedor (integer cliente, integer planta, long planilla);String Contenedor;

SELECT defe_nrcont INTO :Contenedor FROM dbo.despafrigoen WHERE
	clie_codigo=:cliente AND 
	plde_codigo=:planta AND
	defe_plasag=:planilla GROUP BY defe_nrcont;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla despafrigoen")
	
END IF

RETURN Contenedor;
end function

on w_info_programa_pre_embarque_sag.create
int iCurrent
call super::create
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
this.st_5=create st_5
this.ddlb_sitios=create ddlb_sitios
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.em_fecha
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.em_planilla
this.Control[iCurrent+10]=this.em_fecha_des
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.ddlb_sitios
this.Control[iCurrent+13]=this.dw_3
end on

on w_info_programa_pre_embarque_sag.destroy
call super::destroy
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
destroy(this.st_5)
destroy(this.ddlb_sitios)
destroy(this.dw_3)
end on

event open;x=0
y=0
ii_var =	0

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

dw_3.GetChild("sire_codigo",idwc_sitios)
idwc_sitios.SetTransObject(sqlca)
idwc_sitios.Retrieve()
dw_3.InsertRow(0)
//dw_3.SetItem(1,"sire_codigo",1)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

em_fecha.text	=	String(Today())


//ddlb_sitios.SelectItem(1)
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_programa_pre_embarque_sag
end type

type st_computador from w_para_informes`st_computador within w_info_programa_pre_embarque_sag
end type

type st_usuario from w_para_informes`st_usuario within w_info_programa_pre_embarque_sag
end type

type st_temporada from w_para_informes`st_temporada within w_info_programa_pre_embarque_sag
end type

type p_logo from w_para_informes`p_logo within w_info_programa_pre_embarque_sag
end type

type st_titulo from w_para_informes`st_titulo within w_info_programa_pre_embarque_sag
integer width = 1934
string text = "Programa Pre-Embarque S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_programa_pre_embarque_sag
string tag = "Imprimir Reporte"
integer x = 2299
integer y = 660
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_planta, li_cliente, li_TipoDespacho, li_largo, li_sitio
Long		ll_planilla_sag
String	ls_glosa, ls_items

istr_info.titulo	= 'PLANILLA DE DESPACHO FRUTA SAG'

li_cliente			=	Integer(istr_mant.argumento[1])
li_planta			=	Integer(istr_mant.argumento[2])
ll_planilla_sag	=	Long(em_planilla.Text)

//ls_items				=	ddlb_sitios.text
li_sitio				=	dw_3.GetItemNumber(dw_3.GetRow(),'sire_codigo') 
ls_items			=	idwc_sitios.GetItemString(li_sitio,'sire_nombre')


li_TipoDespacho=BuscaTipoDespacho(li_Cliente,li_Planta,ll_Planilla_sag)

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_programa_pre_embarque_sag"

vinf.dw_1.GetChild('embq_ptoori', idwc_pueremb)
idwc_pueremb.SetTransObject(SQLCA)
idwc_pueremb.Retrieve(-1)

vinf.dw_1.GetChild('puer_codigo', idwc_puertos)
idwc_puertos.SetTransObject(SQLCA)
idwc_puertos.Retrieve(-1)

vinf.dw_1.GetChild('dest_codigo', idwc_mercado)
idwc_mercado.SetTransObject(SQLCA)
idwc_mercado.Retrieve(-1)

vinf.dw_1.SetTransObject(SQLCA)

li_fila			=	vinf.dw_1.Retrieve(li_cliente,li_planta,ll_planilla_sag)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	li_largo	=	17
	li_largo	=	li_largo	-	vinf.dw_1.rowcount()
	IF li_largo > 0 THEN
		FOR li_fila	= li_fila + 1 TO li_largo			
			vinf.dw_1.InsertRow(li_fila)
		NEXT	
	END IF

	vinf.dw_1.Modify("t_sitio.text = '" + ls_items + "'")
	
	IF gs_Ambiente = 'Windows' THEN
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
	ELSE
		F_ImprimeInformePdf(vinf.dw_1,istr_info.titulo)
	END IF
	
//	IF rb_prod_agri.Checked=TRUE OR rb_fumig_usa.Checked=TRUE THEN
		rescata_origen(li_cliente,li_planta,ll_Planilla_sag)
//	END IF

END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_programa_pre_embarque_sag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2295
integer y = 940
integer taborder = 60
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_programa_pre_embarque_sag
integer x = 251
integer y = 440
integer width = 1934
integer height = 712
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

type st_1 from statictext within w_info_programa_pre_embarque_sag
integer x = 370
integer y = 756
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

type dw_2 from datawindow within w_info_programa_pre_embarque_sag
integer x = 1010
integer y = 624
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
end event

type st_6 from statictext within w_info_programa_pre_embarque_sag
integer x = 370
integer y = 648
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

type dw_1 from datawindow within w_info_programa_pre_embarque_sag
integer x = 1010
integer y = 736
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

type em_fecha from editmask within w_info_programa_pre_embarque_sag
integer x = 1010
integer y = 512
integer width = 402
integer height = 92
integer taborder = 10
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

type st_3 from statictext within w_info_programa_pre_embarque_sag
integer x = 370
integer y = 528
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

type st_2 from statictext within w_info_programa_pre_embarque_sag
integer x = 370
integer y = 864
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

type em_planilla from editmask within w_info_programa_pre_embarque_sag
event getfocus pbm_ensetfocus
integer x = 1010
integer y = 848
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

type em_fecha_des from editmask within w_info_programa_pre_embarque_sag
integer x = 1577
integer y = 848
integer width = 402
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_5 from statictext within w_info_programa_pre_embarque_sag
integer x = 370
integer y = 980
integer width = 567
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
string text = "Sitio de Inspección"
boolean focusrectangle = false
end type

type ddlb_sitios from dropdownlistbox within w_info_programa_pre_embarque_sag
boolean visible = false
integer x = 562
integer y = 1204
integer width = 983
integer height = 400
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string item[] = {"VALPARAISO LOS CARRERAS","AEROPUERTO SANTIAGO","RANCAGUA LOS LIRIOS"}
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_info_programa_pre_embarque_sag
integer x = 1010
integer y = 964
integer width = 1161
integer height = 92
boolean bringtotop = true
string dataobject = "dddw_sitiosrevision"
boolean border = false
boolean livescroll = true
end type

