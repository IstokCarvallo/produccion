$PBExportHeader$w_info_existencia_gestion.srw
forward
global type w_info_existencia_gestion from w_para_informes
end type
type dw_cliente from datawindow within w_info_existencia_gestion
end type
type st_6 from statictext within w_info_existencia_gestion
end type
type st_4 from statictext within w_info_existencia_gestion
end type
type st_especie from statictext within w_info_existencia_gestion
end type
type st_nro2 from statictext within w_info_existencia_gestion
end type
type st_variedad from statictext within w_info_existencia_gestion
end type
type st_productor from statictext within w_info_existencia_gestion
end type
type cbx_status from checkbox within w_info_existencia_gestion
end type
type dw_stat from datawindow within w_info_existencia_gestion
end type
type rb_controltodos from radiobutton within w_info_existencia_gestion
end type
type rb_rechazados from radiobutton within w_info_existencia_gestion
end type
type rb_objetados from radiobutton within w_info_existencia_gestion
end type
type rb_habilitado from radiobutton within w_info_existencia_gestion
end type
type gb_6 from groupbox within w_info_existencia_gestion
end type
type st_2 from statictext within w_info_existencia_gestion
end type
type st_3 from statictext within w_info_existencia_gestion
end type
type dw_categoria from datawindow within w_info_existencia_gestion
end type
type st_5 from statictext within w_info_existencia_gestion
end type
type cbx_categoria from checkbox within w_info_existencia_gestion
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_gestion
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_gestion
end type
type cbx_varirotula from checkbox within w_info_existencia_gestion
end type
type cbx_tipofrio from checkbox within w_info_existencia_gestion
end type
type st_7 from statictext within w_info_existencia_gestion
end type
type dw_excel from datawindow within w_info_existencia_gestion
end type
type cbx_calibre from checkbox within w_info_existencia_gestion
end type
type st_1 from statictext within w_info_existencia_gestion
end type
type st_8 from statictext within w_info_existencia_gestion
end type
type ddlb_calificacion from dropdownlistbox within w_info_existencia_gestion
end type
type cbx_consolcalifi from checkbox within w_info_existencia_gestion
end type
type cbx_todcalifi from checkbox within w_info_existencia_gestion
end type
type st_embalaje from statictext within w_info_existencia_gestion
end type
type em_embalaje from singlelineedit within w_info_existencia_gestion
end type
type cb_buscaembalaje from commandbutton within w_info_existencia_gestion
end type
type cbx_embalaje from checkbox within w_info_existencia_gestion
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_gestion
end type
type tab_1 from tab within w_info_existencia_gestion
end type
type tabpage_1 from userobject within tab_1
end type
type dw_variedad from datawindow within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_variedad dw_variedad
end type
type tabpage_2 from userobject within tab_1
end type
type dw_embalaje from datawindow within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_embalaje dw_embalaje
end type
type tabpage_3 from userobject within tab_1
end type
type dw_palembalaje from datawindow within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_palembalaje dw_palembalaje
end type
type tabpage_4 from userobject within tab_1
end type
type dw_palcondicion from datawindow within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_palcondicion dw_palcondicion
end type
type tabpage_5 from userobject within tab_1
end type
type dw_palcalibre from datawindow within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_palcalibre dw_palcalibre
end type
type tabpage_6 from userobject within tab_1
end type
type dw_palinspeccion from datawindow within tabpage_6
end type
type tabpage_6 from userobject within tab_1
dw_palinspeccion dw_palinspeccion
end type
type tabpage_7 from userobject within tab_1
end type
type dw_predios from datawindow within tabpage_7
end type
type tabpage_7 from userobject within tab_1
dw_predios dw_predios
end type
type tabpage_8 from userobject within tab_1
end type
type dw_listado from datawindow within tabpage_8
end type
type tabpage_8 from userobject within tab_1
dw_listado dw_listado
end type
type tab_1 from tab within w_info_existencia_gestion
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
tabpage_6 tabpage_6
tabpage_7 tabpage_7
tabpage_8 tabpage_8
end type
type pb_recupera from picturebutton within w_info_existencia_gestion
end type
type st_11 from statictext within w_info_existencia_gestion
end type
type uo_selprotocoloprod from uo_seleccion_protocolo within w_info_existencia_gestion
end type
type st_12 from statictext within w_info_existencia_gestion
end type
type uo_selprotocoloplde from uo_seleccion_protocolo within w_info_existencia_gestion
end type
type gb_4 from groupbox within w_info_existencia_gestion
end type
type st_9 from statictext within w_info_existencia_gestion
end type
type cbx_existeactual from checkbox within w_info_existencia_gestion
end type
type em_hora from editmask within w_info_existencia_gestion
end type
type st_13 from statictext within w_info_existencia_gestion
end type
type st_14 from statictext within w_info_existencia_gestion
end type
type rb_apallet from radiobutton within w_info_existencia_gestion
end type
type rb_pucho from radiobutton within w_info_existencia_gestion
end type
type rb_ambas from radiobutton within w_info_existencia_gestion
end type
type gb_3 from groupbox within w_info_existencia_gestion
end type
type st_10 from statictext within w_info_existencia_gestion
end type
type st_15 from statictext within w_info_existencia_gestion
end type
type cbx_packing from checkbox within w_info_existencia_gestion
end type
type pb_1 from picturebutton within w_info_existencia_gestion
end type
type pb_2 from picturebutton within w_info_existencia_gestion
end type
type gb_5 from groupbox within w_info_existencia_gestion
end type
type gb_7 from groupbox within w_info_existencia_gestion
end type
end forward

global type w_info_existencia_gestion from w_para_informes
integer x = 14
integer y = 32
integer width = 4818
integer height = 3328
string title = "EXISTENCIAS FRIGORIFICOS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_cliente dw_cliente
st_6 st_6
st_4 st_4
st_especie st_especie
st_nro2 st_nro2
st_variedad st_variedad
st_productor st_productor
cbx_status cbx_status
dw_stat dw_stat
rb_controltodos rb_controltodos
rb_rechazados rb_rechazados
rb_objetados rb_objetados
rb_habilitado rb_habilitado
gb_6 gb_6
st_2 st_2
st_3 st_3
dw_categoria dw_categoria
st_5 st_5
cbx_categoria cbx_categoria
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_tipofrio cbx_tipofrio
st_7 st_7
dw_excel dw_excel
cbx_calibre cbx_calibre
st_1 st_1
st_8 st_8
ddlb_calificacion ddlb_calificacion
cbx_consolcalifi cbx_consolcalifi
cbx_todcalifi cbx_todcalifi
st_embalaje st_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_embalaje cbx_embalaje
uo_selproductor uo_selproductor
tab_1 tab_1
pb_recupera pb_recupera
st_11 st_11
uo_selprotocoloprod uo_selprotocoloprod
st_12 st_12
uo_selprotocoloplde uo_selprotocoloplde
gb_4 gb_4
st_9 st_9
cbx_existeactual cbx_existeactual
em_hora em_hora
st_13 st_13
st_14 st_14
rb_apallet rb_apallet
rb_pucho rb_pucho
rb_ambas rb_ambas
gb_3 gb_3
st_10 st_10
st_15 st_15
cbx_packing cbx_packing
pb_1 pb_1
pb_2 pb_2
gb_5 gb_5
gb_7 gb_7
end type
global w_info_existencia_gestion w_info_existencia_gestion

type variables
str_busqueda 	istr_busq
str_mant 		istr_mant

String is_control_d
Integer ii_control, ii_calificacion

DataWindowChild	idwc_cliente, idwc_especie, idwc_stat, idwc_categorias

DataWindow			dw_1,dw_2,dw_3,dw_4,dw_5,dw_6,dw_7,dw_8

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor

end variables

forward prototypes
public function boolean noexistecliente (string cliente)
public function boolean noexistecategoria (integer categoria)
public subroutine recupera_listadopallet (integer ai_pallet)
end prototypes

public function boolean noexistecliente (string cliente);Integer		li_cliente
String		ls_nombre

li_cliente		=	Integer(cliente)

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_cliente.SetItem(1, "clie_codigo", li_cliente)
	istr_mant.argumento[1]	=	String(li_cliente)	
	RETURN False
END IF
end function

public function boolean noexistecategoria (integer categoria);Integer		li_categoria
String		ls_nombre

li_categoria		=	Integer(categoria)

SELECT	cate_nombre
	INTO	:ls_nombre
	FROM	dba.categorias
	WHERE	cate_codigo	=	:li_categoria;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Categorias")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Categoria no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine recupera_listadopallet (integer ai_pallet);Integer	fila,li_Cliente, li_categoria, li_varirotula, li_tipofrio,li_packing, li_calibre,&
			li_emba, li_emba2, li_existencia, li_pallet
String	ls_descri, ls_cliente, ls_categoria, ls_Archivo, ls_ruta, ls_lista, ls_embalaje,&
			ls_string, ls_construye, ls_construyelike1, ls_construyelike
Long		ll_produ
boolean 	lb_flag_retrieve
DateTime	ldt_fecha

	SetPointer(Arrow!)
	pb_acepta.enabled = True
	ls_embalaje	=	istr_mant.argumento[8]+','
	li_emba = len(ls_embalaje)
	
	FOR li_emba2 = 1 TO li_emba
		ls_string = mid(ls_embalaje,li_emba2,1)
		
		IF ls_string <> ',' THEN
			ls_construye = ls_construye+ls_string
		ELSE
			IF ls_construyelike1 = '' THEN
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
	
	IF cbx_embalaje.Checked THEN
		ls_construyelike = "'Z' = 'Z'"
	END IF
	
	/*
	Especies
	*/
	IF IsNull(uo_selespecie.Codigo)THEN
		MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
		uo_selespecie.dw_Seleccion.SetFocus()
		RETURN
	END IF
	
	/*
	calificación
	*/
	IF ii_calificacion = 0 THEN
		MessageBox("Atención","Debe Seleccionar una Calificación Previamente",Exclamation!)
		ddlb_calificacion.SetFocus()
		RETURN
	END IF	
	/*
	productor
	*/
	ls_lista = uo_selproductor.Lista
	
	/*
	Variedad
	*/
	IF IsNull(uo_selvariedad.Codigo)THEN
		MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
		uo_selvariedad.dw_Seleccion.SetFocus()
		RETURN
	END IF
	
	IF cbx_varirotula.Checked THEN
		li_varirotula = 1
	ELSE
		li_varirotula = 0
	END IF
	
	IF cbx_calibre.Checked THEN
		li_calibre = 1
	ELSE
		li_calibre = 0
	END IF
	
	IF cbx_tipofrio.Checked THEN
		li_tipofrio = 1
	ELSE
		li_tipofrio = 0
	END IF
	
	IF cbx_packing.Checked THEN
		li_packing = -9
	ELSE
		li_packing = 1
	END IF	
	
	li_Cliente		=	Integer(istr_mant.argumento[1])
	li_categoria	=	Integer(istr_mant.argumento[10])
	
	SELECT clie_nombre  
		INTO :ls_cliente  
		FROM dba.clientesprod  
		WHERE clie_codigo= :li_Cliente ;
		
	SELECT cate_nombre  
		INTO :ls_categoria  
		FROM dba.categorias 
		WHERE cate_codigo= :li_categoria;	
	
	IF cbx_existeactual.Checked THEN	
		ldt_fecha = Datetime(Date(Today()),Time(Today())	)
		li_existencia = 1
	ELSE	
		ldt_fecha = DateTime(em_hora.Text)
		li_existencia = 0
	END IF	
	
		ll_produ		=	Long(istr_mant.argumento[4])
		
		fila = dw_8.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia,ai_pallet)		
		
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		ELSE
			IF ls_lista <> '-1' AND ls_lista <> '-9' THEN
				ls_descri	=	ls_lista			
			ELSE
				ls_descri	=	istr_mant.argumento[5]
			END IF	
			
			F_Membrete(dw_8)
			dw_8.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_8.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_8.Modify("productor.text = '" + ls_descri+ "'")
			dw_8.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_8.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_8.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_8.Modify("categoria.text = '" + ls_categoria + "'"	)
		
			Visible	= True
			Enabled	= True
			
			pb_acepta.enabled = True
		END IF
	SetPointer(Arrow!)				




end subroutine

on w_info_existencia_gestion.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.st_productor=create st_productor
this.cbx_status=create cbx_status
this.dw_stat=create dw_stat
this.rb_controltodos=create rb_controltodos
this.rb_rechazados=create rb_rechazados
this.rb_objetados=create rb_objetados
this.rb_habilitado=create rb_habilitado
this.gb_6=create gb_6
this.st_2=create st_2
this.st_3=create st_3
this.dw_categoria=create dw_categoria
this.st_5=create st_5
this.cbx_categoria=create cbx_categoria
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_tipofrio=create cbx_tipofrio
this.st_7=create st_7
this.dw_excel=create dw_excel
this.cbx_calibre=create cbx_calibre
this.st_1=create st_1
this.st_8=create st_8
this.ddlb_calificacion=create ddlb_calificacion
this.cbx_consolcalifi=create cbx_consolcalifi
this.cbx_todcalifi=create cbx_todcalifi
this.st_embalaje=create st_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_embalaje=create cbx_embalaje
this.uo_selproductor=create uo_selproductor
this.tab_1=create tab_1
this.pb_recupera=create pb_recupera
this.st_11=create st_11
this.uo_selprotocoloprod=create uo_selprotocoloprod
this.st_12=create st_12
this.uo_selprotocoloplde=create uo_selprotocoloplde
this.gb_4=create gb_4
this.st_9=create st_9
this.cbx_existeactual=create cbx_existeactual
this.em_hora=create em_hora
this.st_13=create st_13
this.st_14=create st_14
this.rb_apallet=create rb_apallet
this.rb_pucho=create rb_pucho
this.rb_ambas=create rb_ambas
this.gb_3=create gb_3
this.st_10=create st_10
this.st_15=create st_15
this.cbx_packing=create cbx_packing
this.pb_1=create pb_1
this.pb_2=create pb_2
this.gb_5=create gb_5
this.gb_7=create gb_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_especie
this.Control[iCurrent+5]=this.st_nro2
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_productor
this.Control[iCurrent+8]=this.cbx_status
this.Control[iCurrent+9]=this.dw_stat
this.Control[iCurrent+10]=this.rb_controltodos
this.Control[iCurrent+11]=this.rb_rechazados
this.Control[iCurrent+12]=this.rb_objetados
this.Control[iCurrent+13]=this.rb_habilitado
this.Control[iCurrent+14]=this.gb_6
this.Control[iCurrent+15]=this.st_2
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.dw_categoria
this.Control[iCurrent+18]=this.st_5
this.Control[iCurrent+19]=this.cbx_categoria
this.Control[iCurrent+20]=this.uo_selespecie
this.Control[iCurrent+21]=this.uo_selvariedad
this.Control[iCurrent+22]=this.cbx_varirotula
this.Control[iCurrent+23]=this.cbx_tipofrio
this.Control[iCurrent+24]=this.st_7
this.Control[iCurrent+25]=this.dw_excel
this.Control[iCurrent+26]=this.cbx_calibre
this.Control[iCurrent+27]=this.st_1
this.Control[iCurrent+28]=this.st_8
this.Control[iCurrent+29]=this.ddlb_calificacion
this.Control[iCurrent+30]=this.cbx_consolcalifi
this.Control[iCurrent+31]=this.cbx_todcalifi
this.Control[iCurrent+32]=this.st_embalaje
this.Control[iCurrent+33]=this.em_embalaje
this.Control[iCurrent+34]=this.cb_buscaembalaje
this.Control[iCurrent+35]=this.cbx_embalaje
this.Control[iCurrent+36]=this.uo_selproductor
this.Control[iCurrent+37]=this.tab_1
this.Control[iCurrent+38]=this.pb_recupera
this.Control[iCurrent+39]=this.st_11
this.Control[iCurrent+40]=this.uo_selprotocoloprod
this.Control[iCurrent+41]=this.st_12
this.Control[iCurrent+42]=this.uo_selprotocoloplde
this.Control[iCurrent+43]=this.gb_4
this.Control[iCurrent+44]=this.st_9
this.Control[iCurrent+45]=this.cbx_existeactual
this.Control[iCurrent+46]=this.em_hora
this.Control[iCurrent+47]=this.st_13
this.Control[iCurrent+48]=this.st_14
this.Control[iCurrent+49]=this.rb_apallet
this.Control[iCurrent+50]=this.rb_pucho
this.Control[iCurrent+51]=this.rb_ambas
this.Control[iCurrent+52]=this.gb_3
this.Control[iCurrent+53]=this.st_10
this.Control[iCurrent+54]=this.st_15
this.Control[iCurrent+55]=this.cbx_packing
this.Control[iCurrent+56]=this.pb_1
this.Control[iCurrent+57]=this.pb_2
this.Control[iCurrent+58]=this.gb_5
this.Control[iCurrent+59]=this.gb_7
end on

on w_info_existencia_gestion.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.st_productor)
destroy(this.cbx_status)
destroy(this.dw_stat)
destroy(this.rb_controltodos)
destroy(this.rb_rechazados)
destroy(this.rb_objetados)
destroy(this.rb_habilitado)
destroy(this.gb_6)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.dw_categoria)
destroy(this.st_5)
destroy(this.cbx_categoria)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_tipofrio)
destroy(this.st_7)
destroy(this.dw_excel)
destroy(this.cbx_calibre)
destroy(this.st_1)
destroy(this.st_8)
destroy(this.ddlb_calificacion)
destroy(this.cbx_consolcalifi)
destroy(this.cbx_todcalifi)
destroy(this.st_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_embalaje)
destroy(this.uo_selproductor)
destroy(this.tab_1)
destroy(this.pb_recupera)
destroy(this.st_11)
destroy(this.uo_selprotocoloprod)
destroy(this.st_12)
destroy(this.uo_selprotocoloplde)
destroy(this.gb_4)
destroy(this.st_9)
destroy(this.cbx_existeactual)
destroy(this.em_hora)
destroy(this.st_13)
destroy(this.st_14)
destroy(this.rb_apallet)
destroy(this.rb_pucho)
destroy(this.rb_ambas)
destroy(this.gb_3)
destroy(this.st_10)
destroy(this.st_15)
destroy(this.cbx_packing)
destroy(this.pb_1)
destroy(this.pb_2)
destroy(this.gb_5)
destroy(this.gb_7)
end on

event open;call super::open;Boolean lb_Cerrar

em_hora.Text	 = String(Today(), "dd/mm/yyyy hh:mm")

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_codexport)

dw_stat.GetChild("stat_codigo", idwc_stat)
idwc_stat.SetTransObject(SQLCA)
idwc_stat.Retrieve()
dw_stat.InsertRow(0)

dw_categoria.GetChild("cate_codigo", idwc_categorias)
idwc_categorias.SetTransObject(SQLCA)
idwc_categorias.Retrieve()
dw_categoria.InsertRow(0)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,False)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

// uo_seleccion_protocolo prod
IF IsNull(uo_selprotocoloprod.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selprotocoloprod.Seleccion(True,True)
END IF

// uo_seleccion_protocolo plde
IF IsNull(uo_selprotocoloplde.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selprotocoloplde.Seleccion(True,True)
END IF

dw_1	=	tab_1.tabpage_1.dw_variedad
dw_2	=	tab_1.tabpage_2.dw_embalaje
dw_3	=	tab_1.tabpage_3.dw_palembalaje
dw_4	=	tab_1.tabpage_4.dw_palcondicion
dw_5	=	tab_1.tabpage_5.dw_palcalibre
dw_6	=	tab_1.tabpage_6.dw_palinspeccion
dw_7	=	tab_1.tabpage_7.dw_predios
dw_8	=	tab_1.tabpage_8.dw_listado

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
dw_4.SetTransObject(Sqlca)
dw_5.SetTransObject(Sqlca)
dw_6.SetTransObject(Sqlca)
dw_7.SetTransObject(Sqlca)
dw_8.SetTransObject(Sqlca)

ii_calificacion = -9

istr_mant.argumento[1]	= 	String(gi_codexport)		//	Cliente
//istr_mant.argumento[2]	= 	String(gi_codespecie)	//	Especie
//istr_mant.argumento[3]	=	'0'							// Variedad
istr_mant.argumento[5]	=	'Todos'						// Descrip Prod
istr_mant.argumento[6]  =	'0'							// status
istr_mant.argumento[7]	=  "Todos"						// status nombre
istr_mant.argumento[10] =	'-9'							// Categorias
istr_mant.argumento[8]	=	'Z'							// Embalaje

dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)

ii_control 					= 	-1								// Control Calidad
is_control_d				= "Todos"						// Valor para Titulo

pb_recupera.PostEvent(clicked!)
end event

event resize;//pb_acepta.x			=	This.WorkSpaceWidth() - 292
//pb_acepta.y			=	gb_1.y + 88
//pb_acepta.width	=	156
//pb_acepta.height	=	133
end event

type st_computador from w_para_informes`st_computador within w_info_existencia_gestion
boolean visible = false
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_gestion
boolean visible = false
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_gestion
boolean visible = false
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_gestion
boolean visible = false
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_gestion
boolean visible = false
integer x = 2267
integer y = 2000
integer width = 2990
string text = "Existencia Almacenada en Frigorificos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_gestion
string tag = "Imprimir Reporte"
integer x = 4498
integer y = 1376
integer taborder = 90
integer textsize = -7
fontcharset fontcharset = ansi!
boolean enabled = false
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_Cliente, li_categoria, li_varirotula, li_tipofrio,li_packing, li_calibre,&
			li_emba, li_emba2, li_existencia, li_pallet
String	ls_descri, ls_cliente, ls_categoria, ls_Archivo, ls_ruta, ls_lista, ls_embalaje,&
			ls_string, ls_construye, ls_construyelike1, ls_construyelike
Long		ll_produ
boolean 	lb_flag_retrieve
DateTime		ldt_fecha

ls_embalaje	=	istr_mant.argumento[8]+','
li_emba = len(ls_embalaje)

FOR li_emba2 = 1 TO li_emba
	ls_string = mid(ls_embalaje,li_emba2,1)
	
	IF ls_string <> ',' THEN
		ls_construye = ls_construye+ls_string
	ELSE
		IF ls_construyelike1 = '' THEN
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

IF cbx_embalaje.Checked THEN
	ls_construyelike = "'Z' = 'Z'"
END IF

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

/*
calificación
*/
IF ii_calificacion = 0 THEN
	MessageBox("Atención","Debe Seleccionar una Calificación Previamente",Exclamation!)
	ddlb_calificacion.SetFocus()
	RETURN
END IF	
/*
productor
*/
ls_lista = uo_selproductor.Lista

/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF cbx_calibre.Checked THEN
	li_calibre = 1
ELSE
	li_calibre = 0
END IF

IF cbx_tipofrio.Checked THEN
	li_tipofrio = 1
ELSE
	li_tipofrio = 0
END IF

IF cbx_packing.Checked THEN
	li_packing = -9
ELSE
	li_packing = 1
END IF	

li_Cliente		=	Integer(istr_mant.argumento[1])
li_categoria	=	Integer(istr_mant.argumento[10])

SELECT clie_nombre  
   INTO :ls_cliente  
	FROM dba.clientesprod  
   WHERE clie_codigo= :li_Cliente ;
	
SELECT cate_nombre  
   INTO :ls_categoria  
	FROM dba.categorias 
   WHERE cate_codigo= :li_categoria;	
	
istr_info.titulo		= 'EXISTENCIA ALMACENADA EN FRIGORIFICOS'

OpenWithParm(vinf, istr_info)

ll_produ		=	Long(istr_mant.argumento[4])

IF cbx_existeactual.Checked THEN	
	ldt_fecha = Datetime(Date(Today()),Time(Today()))
	li_existencia = 1
ELSE	
	ldt_fecha = DateTime(em_hora.Text)
	li_existencia = 0
END IF	

IF tab_1.SelectedTab = 1 THEN
	vinf.dw_1.DataObject = "dw_existencia_frigovariedad_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)
										 
ELSEIF tab_1.SelectedTab = 2 THEN	
	vinf.dw_1.DataObject = "dw_existencia_frigoembalaje_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)
									 
ELSEIF tab_1.SelectedTab = 3 THEN	
	vinf.dw_1.DataObject = "dw_existencia_frigopalletpucho_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)
											 
ELSEIF tab_1.SelectedTab = 4 THEN
	vinf.dw_1.DataObject = "dw_existencia_frigopalletcondicion_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)
								 
ELSEIF tab_1.SelectedTab = 5 THEN
	vinf.dw_1.DataObject = "dw_existencia_frigopalletpucho_ecal_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)

ELSEIF tab_1.SelectedTab = 6 THEN
	vinf.dw_1.DataObject = "dw_existencia_frigopalletinspeccion_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)

ELSEIF tab_1.SelectedTab = 7 THEN
	vinf.dw_1.DataObject = "dw_existencia_frigovariedad_predio_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)

ELSEIF tab_1.SelectedTab = 8 THEN
	vinf.dw_1.DataObject = "dw_info_existencia_palletpucho_fechas_gestion"
	
	vinf.dw_1.SetTransObject(sqlca)
										 
END IF	

IF tab_1.SelectedTab <> 8 THEN 
	fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)	
ELSE		
	
	 IF rb_apallet.Checked THEN
		li_pallet = 1
	 ELSEIF rb_pucho.Checked THEN
		li_pallet = 2
	 ELSE
		li_pallet = -1
	 END IF	
	
	fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia,li_pallet)	
END IF										 

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					 StopSign!, Ok!)

ELSE
	
	IF ls_lista <> '-1' AND ls_lista <> '-9' THEN
		ls_descri	=	ls_lista			
	ELSE
		ls_descri	=	istr_mant.argumento[5]
	END IF	
	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("cliente.text = '" + ls_Cliente + "'"	)		
	//vinf.dw_1.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
	vinf.dw_1.Modify("productor.text = '" + ls_descri+ "'")
	vinf.dw_1.Modify("status.text = '" + istr_mant.argumento[7] + "'")
	
	IF lb_flag_retrieve then 
		vinf.dw_1.Modify("ccalidad.text = '" + is_control_d + "'")
	END IF
	
	IF cbx_categoria.checked THEN
		ls_categoria = 'Consolidada'
	END IF
	
	vinf.dw_1.Modify("categoria.text = '" + ls_categoria + "'"	)

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
//END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_gestion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 4498
integer y = 1768
integer taborder = 100
integer textsize = -7
fontcharset fontcharset = ansi!
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_cliente from datawindow within w_info_existencia_gestion
integer x = 1582
integer y = 8
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data

IF NoExisteCliente(istr_mant.argumento[1]) THEN
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	dw_cliente.SetFocus()
	RETURN 1
ELSE
	uo_selproductor.Filtra(-1,-1,Integer(data))
END IF

end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_existencia_gestion
integer x = 1262
integer y = 32
integer width = 233
integer height = 56
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_existencia_gestion
integer x = 5
integer width = 4347
integer height = 112
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_especie from statictext within w_info_existencia_gestion
integer x = 46
integer y = 204
integer width = 238
integer height = 96
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_existencia_gestion
integer y = 112
integer width = 1230
integer height = 568
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_existencia_gestion
integer x = 46
integer y = 376
integer width = 279
integer height = 96
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_productor from statictext within w_info_existencia_gestion
integer x = 1280
integer y = 248
integer width = 315
integer height = 96
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_status from checkbox within w_info_existencia_gestion
integer x = 2501
integer y = 376
integer width = 549
integer height = 80
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Status Consolidados    "
end type

event clicked;IF This.Checked THEN
   istr_mant.argumento[6]   =	'1'							// status
   istr_mant.argumento[7]	=  "Consolidados "						// status nombre
ELSE
   istr_mant.argumento[6]   =	'0'							// status
   istr_mant.argumento[7]	=  "Todos "						// status nombre
END IF	
//IF This.Checked THEN
//	dw_stat.Enabled			=	False
//	istr_mant.argumento[6]   =	'0'
//	istr_mant.argumento[7]	=  "Todos"	
//	dw_stat.Reset()
//	dw_stat.InsertRow(0)
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)
//	
//ELSE
//	dw_stat.Enabled			=	True	
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(255, 255, 255)
//	dw_stat.InsertRow(0)
//	dw_stat.SetItem(1, "stat_codigo", 1)
//	istr_mant.argumento[7]	=  "Normal"
//END IF
end event

type dw_stat from datawindow within w_info_existencia_gestion
boolean visible = false
integer x = 178
integer y = 1936
integer width = 969
integer height = 88
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[6]	=	data
istr_mant.argumento[7]	=  f_statnombre(integer(data))

end event

type rb_controltodos from radiobutton within w_info_existencia_gestion
integer x = 87
integer y = 728
integer width = 283
integer height = 72
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_control = -1
	is_control_d = this.text
end if
end event

type rb_rechazados from radiobutton within w_info_existencia_gestion
integer x = 544
integer y = 728
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_control=3
	is_control_d = this.Text
END IF
end event

type rb_objetados from radiobutton within w_info_existencia_gestion
integer x = 1143
integer y = 728
integer width = 407
integer height = 72
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_control=2
	is_control_d = this.Text
END IF
end event

type rb_habilitado from radiobutton within w_info_existencia_gestion
integer x = 1829
integer y = 728
integer width = 434
integer height = 72
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_control=1
	is_control_d = this.Text
END IF
end event

type gb_6 from groupbox within w_info_existencia_gestion
integer x = 18
integer y = 684
integer width = 2441
integer height = 124
integer taborder = 160
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Control Calidad"
end type

type st_2 from statictext within w_info_existencia_gestion
integer y = 680
integer width = 2469
integer height = 144
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_existencia_gestion
integer x = 2469
integer y = 120
integer width = 599
integer height = 184
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_categoria from datawindow within w_info_existencia_gestion
integer x = 1536
integer y = 576
integer width = 878
integer height = 96
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_categorias"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_nulo

SetNull(li_nulo)

IF noexistecategoria(Integer(data)) THEN
	dw_categoria.SetItem(1, "cate_codigo", li_nulo)
	dw_categoria.SetFocus()
	RETURN 1
ELSE
	istr_mant.argumento[10]	=	data
END IF


end event

event itemerror;Return 1
end event

type st_5 from statictext within w_info_existencia_gestion
integer x = 1280
integer y = 584
integer width = 242
integer height = 80
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Categorias"
boolean focusrectangle = false
end type

type cbx_categoria from checkbox within w_info_existencia_gestion
integer x = 1545
integer y = 500
integer width = 571
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN	
	

	dw_categoria.Enabled  			= 	False
	dw_categoria.Reset()
	dw_categoria.insertrow(0)
	//istr_mant.argumento[2]		=	'0'
	
ELSE	
	dw_categoria.Enabled  			= True
	dw_categoria.SetFocus()
	dw_categoria.Reset()
	dw_categoria.InsertRow(0)
	
	//istr_mant.argumento[2]		= 	String(gi_codespecie)	
	dw_categoria.SetFocus()
END IF
end event

type uo_selespecie from uo_seleccion_especie within w_info_existencia_gestion
event destroy ( )
integer x = 306
integer y = 124
integer height = 180
integer taborder = 60
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
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_gestion
event destroy ( )
integer x = 306
integer y = 300
integer taborder = 70
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_gestion
integer x = 2501
integer y = 120
integer width = 544
integer height = 80
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Variedad Rotulada"
end type

type cbx_tipofrio from checkbox within w_info_existencia_gestion
integer x = 2501
integer y = 304
integer width = 503
integer height = 80
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Según Tipo Frío  "
end type

type st_7 from statictext within w_info_existencia_gestion
integer x = 2469
integer y = 296
integer width = 599
integer height = 172
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_excel from datawindow within w_info_existencia_gestion
boolean visible = false
integer x = 1111
integer y = 2664
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cbx_calibre from checkbox within w_info_existencia_gestion
integer x = 2501
integer y = 212
integer width = 535
integer height = 80
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Calibre Rotulado"
end type

type st_1 from statictext within w_info_existencia_gestion
integer x = 1230
integer y = 112
integer width = 1234
integer height = 568
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_existencia_gestion
integer x = 46
integer y = 564
integer width = 357
integer height = 88
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Calificación"
boolean focusrectangle = false
end type

type ddlb_calificacion from dropdownlistbox within w_info_existencia_gestion
integer x = 302
integer y = 564
integer width = 480
integer height = 400
integer taborder = 70
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string item[] = {"1","2","3","4","5"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_calificacion	=	(index)
end event

type cbx_consolcalifi from checkbox within w_info_existencia_gestion
integer x = 786
integer y = 480
integer width = 393
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN	
	
	cbx_todcalifi.Enabled = False
	ddlb_calificacion.SelectItem(0)
	cbx_todcalifi.Checked = True
	ddlb_calificacion.Enabled = False
	ii_calificacion = -9
	
ELSE	
	cbx_todcalifi.Enabled = True
	ii_calificacion = -1
END IF
end event

type cbx_todcalifi from checkbox within w_info_existencia_gestion
integer x = 306
integer y = 480
integer width = 407
integer height = 72
integer taborder = 60
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN	
	
	ii_calificacion = -1
	ddlb_calificacion.SelectItem(0)
	ddlb_calificacion.Enabled = False
ELSE
	ddlb_calificacion.Enabled = True
	ii_calificacion = 0
	
END IF
end event

type st_embalaje from statictext within w_info_existencia_gestion
integer x = 1280
integer y = 412
integer width = 352
integer height = 76
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from singlelineedit within w_info_existencia_gestion
integer x = 1541
integer y = 408
integer width = 297
integer height = 84
integer taborder = 120
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;Integer  li_cliente
String	ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
istr_mant.argumento[8]	=	ls_embalaje
	
	
end event

type cb_buscaembalaje from commandbutton within w_info_existencia_gestion
integer x = 1847
integer y = 416
integer width = 96
integer height = 76
integer taborder = 210
boolean bringtotop = true
integer textsize = -7
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

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[8]	=	lstr_busq.argum[2]
	
END IF
end event

type cbx_embalaje from checkbox within w_info_existencia_gestion
integer x = 1957
integer y = 412
integer width = 265
integer height = 80
integer taborder = 170
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_gestion
integer x = 1541
integer y = 124
integer taborder = 80
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

type tab_1 from tab within w_info_existencia_gestion
integer x = 23
integer y = 824
integer width = 4361
integer height = 1788
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean raggedright = true
boolean focusonbuttondown = true
boolean powertips = true
boolean showpicture = false
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
tabpage_6 tabpage_6
tabpage_7 tabpage_7
tabpage_8 tabpage_8
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.tabpage_5=create tabpage_5
this.tabpage_6=create tabpage_6
this.tabpage_7=create tabpage_7
this.tabpage_8=create tabpage_8
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4,&
this.tabpage_5,&
this.tabpage_6,&
this.tabpage_7,&
this.tabpage_8}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
destroy(this.tabpage_5)
destroy(this.tabpage_6)
destroy(this.tabpage_7)
destroy(this.tabpage_8)
end on

event selectionchanged;IF tab_1.SelectedTab = 8 THEN
	gb_3.Visible			= True
	rb_apallet.Visible	= True
	rb_pucho.Visible		= True
	rb_ambas.Visible		= True
ELSE	
	gb_3.Visible			= False
	rb_apallet.Visible	= False
	rb_pucho.Visible		= False
	rb_ambas.Visible		= False
END IF	
end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Variedad"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Por Variedad"
dw_variedad dw_variedad
end type

on tabpage_1.create
this.dw_variedad=create dw_variedad
this.Control[]={this.dw_variedad}
end on

on tabpage_1.destroy
destroy(this.dw_variedad)
end on

type dw_variedad from datawindow within tabpage_1
integer width = 4320
integer height = 1660
integer taborder = 10
string title = "none"
string dataobject = "dw_existencia_frigovariedad_report_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Embalaje"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Por Embalaje"
dw_embalaje dw_embalaje
end type

on tabpage_2.create
this.dw_embalaje=create dw_embalaje
this.Control[]={this.dw_embalaje}
end on

on tabpage_2.destroy
destroy(this.dw_embalaje)
end on

type dw_embalaje from datawindow within tabpage_2
integer width = 4329
integer height = 1656
integer taborder = 20
string title = "none"
string dataobject = "dw_existencia_frigoembalaje_report_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Pall/Puch/Var. Embalaje"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Pallets/Puchos/Variedad Embalaje"
dw_palembalaje dw_palembalaje
end type

on tabpage_3.create
this.dw_palembalaje=create dw_palembalaje
this.Control[]={this.dw_palembalaje}
end on

on tabpage_3.destroy
destroy(this.dw_palembalaje)
end on

type dw_palembalaje from datawindow within tabpage_3
integer width = 4329
integer height = 1656
integer taborder = 20
string title = "none"
string dataobject = "dw_existencia_frigopalletpucho_rep_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Pall/Puch/Var. Condición"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Pallets/Puchos/Variedad Condición"
dw_palcondicion dw_palcondicion
end type

on tabpage_4.create
this.dw_palcondicion=create dw_palcondicion
this.Control[]={this.dw_palcondicion}
end on

on tabpage_4.destroy
destroy(this.dw_palcondicion)
end on

type dw_palcondicion from datawindow within tabpage_4
integer x = 9
integer width = 4320
integer height = 1656
integer taborder = 20
string title = "none"
string dataobject = "dw_existencia_frigopalletcondi_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_5 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Pall/Puch/Var. Calibre"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Pallets/Puchos/Variedad Calibre"
dw_palcalibre dw_palcalibre
end type

on tabpage_5.create
this.dw_palcalibre=create dw_palcalibre
this.Control[]={this.dw_palcalibre}
end on

on tabpage_5.destroy
destroy(this.dw_palcalibre)
end on

type dw_palcalibre from datawindow within tabpage_5
integer width = 4320
integer height = 1664
integer taborder = 20
string title = "none"
string dataobject = "dw_existencia_frigopalletpucho_cal_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_6 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Pall/Puch/Var. Inspección"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Pallets/Puchos/Variedad Inspección"
dw_palinspeccion dw_palinspeccion
end type

on tabpage_6.create
this.dw_palinspeccion=create dw_palinspeccion
this.Control[]={this.dw_palinspeccion}
end on

on tabpage_6.destroy
destroy(this.dw_palinspeccion)
end on

type dw_palinspeccion from datawindow within tabpage_6
integer width = 4320
integer height = 1660
integer taborder = 20
string title = "none"
string dataobject = "dw_existencia_frigopalletinspec_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_7 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Predio"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Por Predio"
dw_predios dw_predios
end type

on tabpage_7.create
this.dw_predios=create dw_predios
this.Control[]={this.dw_predios}
end on

on tabpage_7.destroy
destroy(this.dw_predios)
end on

type dw_predios from datawindow within tabpage_7
integer width = 4320
integer height = 1656
integer taborder = 20
string title = "none"
string dataobject = "dw_info_frigorifico_predios_cuartel_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_8 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4325
integer height = 1660
long backcolor = 12632256
string text = "Pall/Puch"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
string powertiptext = "Listado de Pallet"
dw_listado dw_listado
end type

on tabpage_8.create
this.dw_listado=create dw_listado
this.Control[]={this.dw_listado}
end on

on tabpage_8.destroy
destroy(this.dw_listado)
end on

type dw_listado from datawindow within tabpage_8
integer width = 4320
integer height = 1636
integer taborder = 30
string title = "none"
string dataobject = "dw_info_existencia_palletpucho_fechas_gestion2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_recupera from picturebutton within w_info_existencia_gestion
integer x = 4498
integer y = 268
integer width = 233
integer height = 196
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\RescatarEnable.png"
alignment htextalign = right!
end type

event clicked;Integer	fila,li_Cliente, li_categoria, li_varirotula, li_tipofrio,li_packing, li_calibre,&
			li_emba, li_emba2, li_existencia, li_pallet
String	ls_descri, ls_cliente, ls_categoria, ls_Archivo, ls_ruta, ls_lista, ls_embalaje,&
			ls_string, ls_construye, ls_construyelike1, ls_construyelike
Long		ll_produ
boolean 	lb_flag_retrieve
DateTime	ldt_fecha

//IF tab_1.Visible	=	True THEN
//	tab_1.Visible	=	False
//   pb_recupera.PictureName= '\desarrollo\bmp\nexte.bmp'
//	pb_acepta.enabled = False	
//ELSE
//	tab_1.Visible	=	True
//   pb_recupera.PictureName= '\desarrollo\bmp\Priore.bmp'
	
	SetPointer(Arrow!)
	pb_acepta.enabled = True
	ls_embalaje	=	istr_mant.argumento[8]+','
	li_emba = len(ls_embalaje)
	
	FOR li_emba2 = 1 TO li_emba
		ls_string = mid(ls_embalaje,li_emba2,1)
		
		IF ls_string <> ',' THEN
			ls_construye = ls_construye+ls_string
		ELSE
			IF ls_construyelike1 = '' THEN
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
	
	IF cbx_embalaje.Checked THEN
		ls_construyelike = "'Z' = 'Z'"
	END IF
	
	/*
	Especies
	*/
	IF IsNull(uo_selespecie.Codigo)THEN
		MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
		uo_selespecie.dw_Seleccion.SetFocus()
		RETURN
	END IF
	
	/*
	calificación
	*/
	IF ii_calificacion = 0 THEN
		MessageBox("Atención","Debe Seleccionar una Calificación Previamente",Exclamation!)
		ddlb_calificacion.SetFocus()
		RETURN
	END IF	
	/*
	productor
	*/
	ls_lista = uo_selproductor.Lista
	
	/*
	Variedad
	*/
	IF IsNull(uo_selvariedad.Codigo)THEN
		MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
		uo_selvariedad.dw_Seleccion.SetFocus()
		RETURN
	END IF
	
	IF cbx_varirotula.Checked THEN
		li_varirotula = 1
	ELSE
		li_varirotula = 0
	END IF
	
	IF cbx_calibre.Checked THEN
		li_calibre = 1
	ELSE
		li_calibre = 0
	END IF
	
	IF cbx_tipofrio.Checked THEN
		li_tipofrio = 1
	ELSE
		li_tipofrio = 0
	END IF
	
	IF cbx_packing.Checked THEN
		li_packing = -9
	ELSE
		li_packing = 1
	END IF	
	
	li_Cliente		=	Integer(istr_mant.argumento[1])
	li_categoria	=	Integer(istr_mant.argumento[10])
	
	SELECT clie_nombre  
		INTO :ls_cliente  
		FROM dba.clientesprod  
		WHERE clie_codigo= :li_Cliente ;
		
	SELECT cate_nombre  
		INTO :ls_categoria  
		FROM dba.categorias 
		WHERE cate_codigo= :li_categoria;	
	
	IF cbx_existeactual.Checked THEN	
		ldt_fecha = Datetime(Date(Today()),Time(Today())	)
		li_existencia = 1
	ELSE	
		ldt_fecha = DateTime(em_hora.Text)
		li_existencia = 0
	END IF	
	
		ll_produ		=	Long(istr_mant.argumento[4])
		
		fila	=	dw_1.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)
										 
	   dw_2.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)
										 
	   dw_3.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)	
										 
		dw_4.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)
										 
		dw_5.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)	
										 
	   dw_6.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)	
		
		 dw_7.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)	
		
       IF rb_apallet.Checked THEN
			li_pallet = 1
		 ELSEIF rb_pucho.Checked THEN
			li_pallet = 2
		 ELSE
			li_pallet = -1
		 END IF	
		 
		 dw_8.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia,li_pallet)		
		
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		ELSE
			IF ls_lista <> '-1' AND ls_lista <> '-9' THEN
				ls_descri	=	ls_lista			
			ELSE
				ls_descri	=	istr_mant.argumento[5]
			END IF	
			
			F_Membrete(dw_1)
			dw_1.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_1.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_1.Modify("productor.text = '" + ls_descri+ "'")
			dw_1.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_1.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_1.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			IF cbx_categoria.checked THEN
				ls_categoria = 'Consolidada'
			END IF
			dw_1.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_2)
			dw_2.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_2.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_2.Modify("productor.text = '" + ls_descri+ "'")
			dw_2.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_2.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_2.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_2.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_3)
			dw_3.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_3.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_3.Modify("productor.text = '" + ls_descri+ "'")
			dw_3.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_3.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_3.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_3.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_4)
			dw_4.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_4.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_4.Modify("productor.text = '" + ls_descri+ "'")
			dw_4.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_4.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_4.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_4.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_5)
			dw_5.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_5.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_5.Modify("productor.text = '" + ls_descri+ "'")
			dw_5.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_5.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_5.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_5.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_6)
			dw_6.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_6.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_6.Modify("productor.text = '" + ls_descri+ "'")
			dw_6.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_6.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_6.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_6.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_7)
			dw_7.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_7.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_7.Modify("productor.text = '" + ls_descri+ "'")
			dw_7.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_7.Modify('DataWindow.Print.Preview = Yes')
			//dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
			IF lb_flag_retrieve then 
				dw_7.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_7.Modify("categoria.text = '" + ls_categoria + "'"	)
			
			F_Membrete(dw_8)
			dw_8.Modify("cliente.text = '" + ls_Cliente + "'"	)		
			dw_8.Modify("productor.text = '" + ls_descri+ " (" + String(ll_produ,'#####')+")" + "'")
			dw_8.Modify("productor.text = '" + ls_descri+ "'")
			dw_8.Modify("status.text = '" + istr_mant.argumento[7] + "'")
			//dw_8.Object.DataWindow.Print.Preview			= 'Yes'
			//dw_8.Object.DataWindow.Print.Preview.Zoom	= 75
			
			dw_8.Object.DataWindow.Header.Height  =0
			
			IF lb_flag_retrieve then 
				dw_8.Modify("ccalidad.text = '" + is_control_d + "'")
			END IF
			
			dw_8.Modify("categoria.text = '" + ls_categoria + "'"	)
		
			Visible	= True
			Enabled	= True
			
			pb_acepta.enabled = True
		END IF
	SetPointer(Arrow!)				
//END IF
end event

type st_11 from statictext within w_info_existencia_gestion
integer x = 3086
integer y = 188
integer width = 251
integer height = 152
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Protocolo Productor"
boolean focusrectangle = false
end type

type uo_selprotocoloprod from uo_seleccion_protocolo within w_info_existencia_gestion
integer x = 3328
integer y = 116
integer taborder = 100
boolean bringtotop = true
end type

on uo_selprotocoloprod.destroy
call uo_seleccion_protocolo::destroy
end on

type st_12 from statictext within w_info_existencia_gestion
integer x = 3095
integer y = 436
integer width = 256
integer height = 152
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Protocolo Packing"
boolean focusrectangle = false
end type

type uo_selprotocoloplde from uo_seleccion_protocolo within w_info_existencia_gestion
integer x = 3333
integer y = 360
integer height = 256
integer taborder = 120
boolean bringtotop = true
end type

on uo_selprotocoloplde.destroy
call uo_seleccion_protocolo::destroy
end on

type gb_4 from groupbox within w_info_existencia_gestion
integer x = 4443
integer y = 180
integer width = 270
integer height = 272
integer taborder = 180
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_9 from statictext within w_info_existencia_gestion
integer x = 3067
integer y = 108
integer width = 1285
integer height = 512
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_existeactual from checkbox within w_info_existencia_gestion
integer x = 2560
integer y = 648
integer width = 498
integer height = 56
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Existencia Actual"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_hora.Enabled = False
ELSE
	em_hora.Enabled = True
	em_hora.Text	 = String(Today(), "dd/mm/yyyy hh:mm")
END IF	
end event

type em_hora from editmask within w_info_existencia_gestion
integer x = 2624
integer y = 712
integer width = 343
integer height = 76
integer taborder = 170
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yy hh:mm"
end type

type st_13 from statictext within w_info_existencia_gestion
integer x = 2491
integer y = 732
integer width = 137
integer height = 44
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Hasta"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_existencia_gestion
integer x = 2469
integer y = 628
integer width = 599
integer height = 196
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_apallet from radiobutton within w_info_existencia_gestion
boolean visible = false
integer x = 3227
integer y = 692
integer width = 283
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Pallet"
end type

event clicked;IF This.Checked THEN
	recupera_listadopallet(1)
END IF



end event

type rb_pucho from radiobutton within w_info_existencia_gestion
boolean visible = false
integer x = 3566
integer y = 692
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Pucho"
end type

event clicked;IF This.Checked THEN
	recupera_listadopallet(2)
END IF
end event

type rb_ambas from radiobutton within w_info_existencia_gestion
boolean visible = false
integer x = 3918
integer y = 692
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Ambas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	recupera_listadopallet(-1)
END IF
end event

type gb_3 from groupbox within w_info_existencia_gestion
boolean visible = false
integer x = 3109
integer y = 640
integer width = 1207
integer height = 148
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_10 from statictext within w_info_existencia_gestion
integer x = 3067
integer y = 620
integer width = 1285
integer height = 204
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_existencia_gestion
integer x = 2469
integer y = 468
integer width = 599
integer height = 156
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_existencia_gestion
integer x = 2501
integer y = 508
integer width = 549
integer height = 80
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Packing"
end type

event clicked;IF This.Checked THEN
   istr_mant.argumento[6]   =	'1'							// status
   istr_mant.argumento[7]	=  "Consolidados "						// status nombre
ELSE
   istr_mant.argumento[6]   =	'0'							// status
   istr_mant.argumento[7]	=  "Todos "						// status nombre
END IF	
//IF This.Checked THEN
//	dw_stat.Enabled			=	False
//	istr_mant.argumento[6]   =	'0'
//	istr_mant.argumento[7]	=  "Todos"	
//	dw_stat.Reset()
//	dw_stat.InsertRow(0)
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)
//	
//ELSE
//	dw_stat.Enabled			=	True	
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(255, 255, 255)
//	dw_stat.InsertRow(0)
//	dw_stat.SetItem(1, "stat_codigo", 1)
//	istr_mant.argumento[7]	=  "Normal"
//END IF
end event

type pb_1 from picturebutton within w_info_existencia_gestion
integer x = 4498
integer y = 568
integer width = 233
integer height = 196
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\NuevoEnab.png"
alignment htextalign = right!
end type

event clicked;dw_1.Reset()
dw_2.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
end event

type pb_2 from picturebutton within w_info_existencia_gestion
integer x = 4512
integer y = 2204
integer width = 151
integer height = 132
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo\Bmp\excele.bmp"
alignment htextalign = right!
end type

event clicked;SetPointer(Arrow!)

Integer	fila,li_Cliente, li_categoria, li_varirotula, li_tipofrio,li_packing, li_calibre,&
			li_emba, li_emba2, li_existencia, li_pallet
String	ls_descri, ls_cliente, ls_categoria, ls_Archivo, ls_ruta, ls_lista, ls_embalaje,&
			ls_string, ls_construye, ls_construyelike1, ls_construyelike
Long		ll_produ
boolean 	lb_flag_retrieve
DateTime		ldt_fecha

ls_embalaje	=	istr_mant.argumento[8]+','
li_emba = len(ls_embalaje)

FOR li_emba2 = 1 TO li_emba
	ls_string = mid(ls_embalaje,li_emba2,1)
	
	IF ls_string <> ',' THEN
		ls_construye = ls_construye+ls_string
	ELSE
		IF ls_construyelike1 = '' THEN
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

IF cbx_embalaje.Checked THEN
	ls_construyelike = "'Z' = 'Z'"
END IF

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

/*
calificación
*/
IF ii_calificacion = 0 THEN
	MessageBox("Atención","Debe Seleccionar una Calificación Previamente",Exclamation!)
	ddlb_calificacion.SetFocus()
	RETURN
END IF	
/*
productor
*/
ls_lista = uo_selproductor.Lista

/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF cbx_calibre.Checked THEN
	li_calibre = 1
ELSE
	li_calibre = 0
END IF

IF cbx_tipofrio.Checked THEN
	li_tipofrio = 1
ELSE
	li_tipofrio = 0
END IF

IF cbx_packing.Checked THEN
	li_packing = -9
ELSE
	li_packing = 1
END IF	

li_Cliente		=	Integer(istr_mant.argumento[1])
li_categoria	=	Integer(istr_mant.argumento[10])

SELECT clie_nombre  
   INTO :ls_cliente  
	FROM dba.clientesprod  
   WHERE clie_codigo= :li_Cliente ;
	
SELECT cate_nombre  
   INTO :ls_categoria  
	FROM dba.categorias 
   WHERE cate_codigo= :li_categoria;	
	
ll_produ		=	Long(istr_mant.argumento[4])

IF cbx_existeactual.Checked THEN	
	ldt_fecha = Datetime(Date(Today()),Time(Today()))
	li_existencia = 1
ELSE	
	ldt_fecha = DateTime(em_hora.Text)
	li_existencia = 0
END IF	

IF tab_1.SelectedTab = 1 THEN
	dw_excel.DataObject = "dw_excel_existenciafrigorificos_gestion"
											 
ELSEIF tab_1.SelectedTab = 2 THEN	
	dw_excel.DataObject = "dw_excel_existenciafrigorificos_gestion"
							 
ELSEIF tab_1.SelectedTab = 3 THEN	
	dw_excel.DataObject = "dw_excel_existenciafrigo_palletpucho_gestion"
	
ELSEIF tab_1.SelectedTab = 4 THEN
	dw_excel.DataObject = "dw_excel_existenciafrigo_palletpucho_gestion"
	
ELSEIF tab_1.SelectedTab = 5 THEN
	dw_excel.DataObject = "dw_excel_existenciafrigo_palletpucho_gestion"
	
ELSEIF tab_1.SelectedTab = 6 THEN
	dw_excel.DataObject = "dw_excel_existenciafrigo_pallet_inspec_gestion"
	
ELSEIF tab_1.SelectedTab = 7 THEN
	dw_excel.DataObject = "dw_excel_existenciafrigorificos_predios_gestion"
	
ELSEIF tab_1.SelectedTab = 8 THEN
	dw_excel.DataObject = "dw_info_existencia_palletpucho_fechas_gestion"
	
END IF	

dw_excel.SetTransObject(sqlca)

IF tab_1.SelectedTab <> 8 THEN 
	fila	=	dw_excel.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia)	
ELSE		
	
	 IF rb_apallet.Checked THEN
		li_pallet = 1
	 ELSEIF rb_pucho.Checked THEN
		li_pallet = 2
	 ELSE
		li_pallet = -1
	 END IF	
	
	fila	=	dw_excel.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
										 uo_selvariedad.Codigo, &
										 integer(istr_mant.argumento[6]), ii_control, &
										 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
										 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
										 ldt_fecha,uo_selprotocoloprod.Codigo,uo_selprotocoloplde.Codigo,&
										 uo_selprotocoloprod.certifica,uo_selprotocoloplde.certifica,li_existencia,li_pallet)	
END IF										 

dw_excel.SaveAs("",Excel!, True)

end event

type gb_5 from groupbox within w_info_existencia_gestion
integer x = 4443
integer y = 484
integer width = 270
integer height = 272
integer taborder = 30
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_7 from groupbox within w_info_existencia_gestion
integer x = 4443
integer y = 2116
integer width = 270
integer height = 272
integer taborder = 40
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

