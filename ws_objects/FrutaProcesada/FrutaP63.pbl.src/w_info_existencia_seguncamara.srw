$PBExportHeader$w_info_existencia_seguncamara.srw
forward
global type w_info_existencia_seguncamara from w_para_informes
end type
type dw_cliente from datawindow within w_info_existencia_seguncamara
end type
type st_6 from statictext within w_info_existencia_seguncamara
end type
type st_4 from statictext within w_info_existencia_seguncamara
end type
type st_especie from statictext within w_info_existencia_seguncamara
end type
type st_nro2 from statictext within w_info_existencia_seguncamara
end type
type st_variedad from statictext within w_info_existencia_seguncamara
end type
type st_nro3 from statictext within w_info_existencia_seguncamara
end type
type rb_variedad from radiobutton within w_info_existencia_seguncamara
end type
type rb_embalaje from radiobutton within w_info_existencia_seguncamara
end type
type st_productor from statictext within w_info_existencia_seguncamara
end type
type dw_stat from datawindow within w_info_existencia_seguncamara
end type
type dw_1 from datawindow within w_info_existencia_seguncamara
end type
type dw_2 from datawindow within w_info_existencia_seguncamara
end type
type dw_3 from datawindow within w_info_existencia_seguncamara
end type
type dw_4 from datawindow within w_info_existencia_seguncamara
end type
type dw_5 from datawindow within w_info_existencia_seguncamara
end type
type dw_6 from datawindow within w_info_existencia_seguncamara
end type
type rb_controltodos from radiobutton within w_info_existencia_seguncamara
end type
type rb_rechazados from radiobutton within w_info_existencia_seguncamara
end type
type rb_objetados from radiobutton within w_info_existencia_seguncamara
end type
type rb_habilitado from radiobutton within w_info_existencia_seguncamara
end type
type gb_6 from groupbox within w_info_existencia_seguncamara
end type
type st_2 from statictext within w_info_existencia_seguncamara
end type
type dw_categoria from datawindow within w_info_existencia_seguncamara
end type
type st_5 from statictext within w_info_existencia_seguncamara
end type
type cbx_categoria from checkbox within w_info_existencia_seguncamara
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_seguncamara
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_seguncamara
end type
type cbx_varirotula from checkbox within w_info_existencia_seguncamara
end type
type st_9 from statictext within w_info_existencia_seguncamara
end type
type cbx_archivo from checkbox within w_info_existencia_seguncamara
end type
type dw_excel from datawindow within w_info_existencia_seguncamara
end type
type cbx_calibre from checkbox within w_info_existencia_seguncamara
end type
type st_1 from statictext within w_info_existencia_seguncamara
end type
type st_8 from statictext within w_info_existencia_seguncamara
end type
type ddlb_calificacion from dropdownlistbox within w_info_existencia_seguncamara
end type
type cbx_consolcalifi from checkbox within w_info_existencia_seguncamara
end type
type cbx_todcalifi from checkbox within w_info_existencia_seguncamara
end type
type st_embalaje from statictext within w_info_existencia_seguncamara
end type
type em_embalaje from singlelineedit within w_info_existencia_seguncamara
end type
type cb_buscaembalaje from commandbutton within w_info_existencia_seguncamara
end type
type cbx_embalaje from checkbox within w_info_existencia_seguncamara
end type
type st_7 from statictext within w_info_existencia_seguncamara
end type
type dw_camara from datawindow within w_info_existencia_seguncamara
end type
type cbx_camaras from checkbox within w_info_existencia_seguncamara
end type
type dw_tipocamara from datawindow within w_info_existencia_seguncamara
end type
type st_10 from statictext within w_info_existencia_seguncamara
end type
type cbx_1 from checkbox within w_info_existencia_seguncamara
end type
type st_11 from statictext within w_info_existencia_seguncamara
end type
type dw_planta from datawindow within w_info_existencia_seguncamara
end type
type cbx_planta from checkbox within w_info_existencia_seguncamara
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_seguncamara
end type
type cbx_clientes from checkbox within w_info_existencia_seguncamara
end type
end forward

global type w_info_existencia_seguncamara from w_para_informes
integer x = 14
integer y = 32
integer width = 3762
integer height = 2160
string title = "Existencia de Fruta"
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
st_nro3 st_nro3
rb_variedad rb_variedad
rb_embalaje rb_embalaje
st_productor st_productor
dw_stat dw_stat
dw_1 dw_1
dw_2 dw_2
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
rb_controltodos rb_controltodos
rb_rechazados rb_rechazados
rb_objetados rb_objetados
rb_habilitado rb_habilitado
gb_6 gb_6
st_2 st_2
dw_categoria dw_categoria
st_5 st_5
cbx_categoria cbx_categoria
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_9 st_9
cbx_archivo cbx_archivo
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
st_7 st_7
dw_camara dw_camara
cbx_camaras cbx_camaras
dw_tipocamara dw_tipocamara
st_10 st_10
cbx_1 cbx_1
st_11 st_11
dw_planta dw_planta
cbx_planta cbx_planta
uo_selproductor uo_selproductor
cbx_clientes cbx_clientes
end type
global w_info_existencia_seguncamara w_info_existencia_seguncamara

type variables
str_busqueda 	istr_busq
str_mant 		istr_mant

String is_control_d
Integer ii_control, ii_calificacion, ii_camara, ii_tipocamara

DataWindowChild	idwc_cliente, idwc_especie, idwc_stat, idwc_categorias, idwc_camara, idwc_tipcamara, idwc_planta

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor

end variables

forward prototypes
public function boolean noexistecliente (string cliente)
public function boolean noexistecategoria (integer categoria)
end prototypes

public function boolean noexistecliente (string cliente);Integer		li_cliente
String		ls_nombre

li_cliente		=	Integer(cliente)

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dbo.clientesprod
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
	FROM	dbo.categorias
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

on w_info_existencia_seguncamara.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.st_nro3=create st_nro3
this.rb_variedad=create rb_variedad
this.rb_embalaje=create rb_embalaje
this.st_productor=create st_productor
this.dw_stat=create dw_stat
this.dw_1=create dw_1
this.dw_2=create dw_2
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.rb_controltodos=create rb_controltodos
this.rb_rechazados=create rb_rechazados
this.rb_objetados=create rb_objetados
this.rb_habilitado=create rb_habilitado
this.gb_6=create gb_6
this.st_2=create st_2
this.dw_categoria=create dw_categoria
this.st_5=create st_5
this.cbx_categoria=create cbx_categoria
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_9=create st_9
this.cbx_archivo=create cbx_archivo
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
this.st_7=create st_7
this.dw_camara=create dw_camara
this.cbx_camaras=create cbx_camaras
this.dw_tipocamara=create dw_tipocamara
this.st_10=create st_10
this.cbx_1=create cbx_1
this.st_11=create st_11
this.dw_planta=create dw_planta
this.cbx_planta=create cbx_planta
this.uo_selproductor=create uo_selproductor
this.cbx_clientes=create cbx_clientes
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_especie
this.Control[iCurrent+5]=this.st_nro2
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_nro3
this.Control[iCurrent+8]=this.rb_variedad
this.Control[iCurrent+9]=this.rb_embalaje
this.Control[iCurrent+10]=this.st_productor
this.Control[iCurrent+11]=this.dw_stat
this.Control[iCurrent+12]=this.dw_1
this.Control[iCurrent+13]=this.dw_2
this.Control[iCurrent+14]=this.dw_3
this.Control[iCurrent+15]=this.dw_4
this.Control[iCurrent+16]=this.dw_5
this.Control[iCurrent+17]=this.dw_6
this.Control[iCurrent+18]=this.rb_controltodos
this.Control[iCurrent+19]=this.rb_rechazados
this.Control[iCurrent+20]=this.rb_objetados
this.Control[iCurrent+21]=this.rb_habilitado
this.Control[iCurrent+22]=this.gb_6
this.Control[iCurrent+23]=this.st_2
this.Control[iCurrent+24]=this.dw_categoria
this.Control[iCurrent+25]=this.st_5
this.Control[iCurrent+26]=this.cbx_categoria
this.Control[iCurrent+27]=this.uo_selespecie
this.Control[iCurrent+28]=this.uo_selvariedad
this.Control[iCurrent+29]=this.cbx_varirotula
this.Control[iCurrent+30]=this.st_9
this.Control[iCurrent+31]=this.cbx_archivo
this.Control[iCurrent+32]=this.dw_excel
this.Control[iCurrent+33]=this.cbx_calibre
this.Control[iCurrent+34]=this.st_1
this.Control[iCurrent+35]=this.st_8
this.Control[iCurrent+36]=this.ddlb_calificacion
this.Control[iCurrent+37]=this.cbx_consolcalifi
this.Control[iCurrent+38]=this.cbx_todcalifi
this.Control[iCurrent+39]=this.st_embalaje
this.Control[iCurrent+40]=this.em_embalaje
this.Control[iCurrent+41]=this.cb_buscaembalaje
this.Control[iCurrent+42]=this.cbx_embalaje
this.Control[iCurrent+43]=this.st_7
this.Control[iCurrent+44]=this.dw_camara
this.Control[iCurrent+45]=this.cbx_camaras
this.Control[iCurrent+46]=this.dw_tipocamara
this.Control[iCurrent+47]=this.st_10
this.Control[iCurrent+48]=this.cbx_1
this.Control[iCurrent+49]=this.st_11
this.Control[iCurrent+50]=this.dw_planta
this.Control[iCurrent+51]=this.cbx_planta
this.Control[iCurrent+52]=this.uo_selproductor
this.Control[iCurrent+53]=this.cbx_clientes
end on

on w_info_existencia_seguncamara.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.st_nro3)
destroy(this.rb_variedad)
destroy(this.rb_embalaje)
destroy(this.st_productor)
destroy(this.dw_stat)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.rb_controltodos)
destroy(this.rb_rechazados)
destroy(this.rb_objetados)
destroy(this.rb_habilitado)
destroy(this.gb_6)
destroy(this.st_2)
destroy(this.dw_categoria)
destroy(this.st_5)
destroy(this.cbx_categoria)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_9)
destroy(this.cbx_archivo)
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
destroy(this.st_7)
destroy(this.dw_camara)
destroy(this.cbx_camaras)
destroy(this.dw_tipocamara)
destroy(this.st_10)
destroy(this.cbx_1)
destroy(this.st_11)
destroy(this.dw_planta)
destroy(this.cbx_planta)
destroy(this.uo_selproductor)
destroy(this.cbx_clientes)
end on

event open;call super::open;Boolean lb_Cerrar

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

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_codplanta)

dw_camara.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SQLCA)
idwc_camara.Retrieve(gi_codplanta,0)
dw_camara.InsertRow(0)

dw_tipocamara.GetChild("cama_tipoca", idwc_tipcamara)
idwc_tipcamara.SetTransObject(SQLCA)
idwc_tipcamara.Retrieve()
dw_tipocamara.InsertRow(0)

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

ii_calificacion = -9

istr_mant.argumento[1]	= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[2]	= 	String(gi_codplanta)		//	Planta
//istr_mant.argumento[3]	=	'0'						// Variedad
istr_mant.argumento[5]	=	'Todos'						// Descrip Prod
istr_mant.argumento[6]  =	'0'							// status
istr_mant.argumento[7]	=  "Todos"						// status nombre
istr_mant.argumento[10] =	'-9'							// Categorias
istr_mant.argumento[8]	=	'Z'							// Embalaje

ii_camara					=	-1							
ii_tipocamara				=	0							

dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)

ii_control 					= 	-1								// Control Calidad
is_control_d				= "Todos"						// Valor para Titulo
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_seguncamara
integer x = 3406
integer y = 884
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_seguncamara
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_seguncamara
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_seguncamara
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_seguncamara
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_seguncamara
integer width = 2990
string text = "Existencia Almacenada en Frigorificos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_seguncamara
string tag = "Imprimir Reporte"
integer x = 3406
integer y = 1292
integer taborder = 230
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_Cliente, li_categoria, li_varirotula, li_tipofrio,li_packing, li_calibre,&
			li_emba, li_emba2
String	ls_descri, ls_cliente, ls_categoria, ls_Archivo, ls_ruta, ls_lista, ls_embalaje,&
			ls_string, ls_construye, ls_construyelike1, ls_construyelike
Long		ll_produ, ll_fila
boolean 	lb_flag_retrieve

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

li_Cliente		=	Integer(istr_mant.argumento[1])
li_categoria	=	Integer(istr_mant.argumento[10])

SELECT clie_nombre  
   INTO :ls_cliente  
	FROM dbo.clientesprod  
   WHERE clie_codigo= :li_Cliente ;
	
SELECT cate_nombre  
   INTO :ls_categoria  
	FROM dbo.categorias 
   WHERE cate_codigo= :li_categoria;	

istr_info.titulo		= 'EXISTENCIA ALMACENADA EN FRIGORIFICOS'

OpenWithParm(vinf, istr_info)

ll_produ		=	Long(istr_mant.argumento[4])

IF rb_variedad.Checked THEN
	vinf.dw_1.DataObject = "dw_existencia_frigopalletpucho_camaravarieded"
ELSE
	vinf.dw_1.DataObject = "dw_existencia_frigopalletpucho_porcamara"
END IF	

vinf.dw_1.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
									 uo_selvariedad.Codigo, &
									 integer(istr_mant.argumento[6]),integer(istr_mant.argumento[10]),&
									 li_varirotula,li_tipofrio,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
									 ii_tipocamara,ii_camara,ii_control)

IF cbx_archivo.Checked THEN	
	ll_fila = dw_3.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
									 uo_selvariedad.Codigo, &
									 integer(istr_mant.argumento[6]),integer(istr_mant.argumento[10]),&
									 li_varirotula,li_tipofrio,li_calibre,ls_lista,ii_calificacion,ls_construyelike,&
									 ii_tipocamara,ii_camara,ii_control)
	IF ll_fila > 0 THEN
		ls_Archivo	=	"\ExistenciaFrigorifico.xls"
			
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
				
		dw_3.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)
		MessageBox("Atención","Archivo Formato Excel, Generado.")
	END IF	
END IF	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
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
		
		IF cbx_1.Checked THEN
			vinf.dw_1.Modify("t_tipocamara.text = '" + 'Todas' + "'")
		ELSE
			IF ii_tipocamara = 1 THEN
				vinf.dw_1.Modify("t_tipocamara.text = '" + 'Pre Frío' + "'")
			ELSEIF ii_tipocamara = 2 THEN
				vinf.dw_1.Modify("t_tipocamara.text = '" + 'Fumigación' + "'")
			ELSEIF ii_tipocamara = 3 THEN
				vinf.dw_1.Modify("t_tipocamara.text = '" + 'Mantención' + "'")
			ELSEIF ii_tipocamara = 0 THEN
				vinf.dw_1.Modify("t_tipocamara.text = '" + 'Patio' + "'")
			END IF	
		END IF
		vinf.dw_1.Modify("ccalidad.text = '" + is_control_d + "'")
		IF cbx_categoria.checked THEN ls_categoria = 'Consolidada'
		vinf.dw_1.Modify("categoria.text = '" + ls_categoria + "'"	)
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_seguncamara
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3387
integer y = 1656
integer taborder = 240
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_cliente from datawindow within w_info_existencia_seguncamara
integer x = 526
integer y = 504
integer width = 1152
integer height = 96
integer taborder = 20
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

type st_6 from statictext within w_info_existencia_seguncamara
integer x = 270
integer y = 516
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

type st_4 from statictext within w_info_existencia_seguncamara
integer x = 242
integer y = 440
integer width = 2990
integer height = 168
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

type st_especie from statictext within w_info_existencia_seguncamara
integer x = 293
integer y = 712
integer width = 238
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_existencia_seguncamara
integer x = 242
integer y = 608
integer width = 1467
integer height = 780
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

type st_variedad from statictext within w_info_existencia_seguncamara
integer x = 288
integer y = 920
integer width = 279
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

type st_nro3 from statictext within w_info_existencia_seguncamara
integer x = 242
integer y = 1388
integer width = 2990
integer height = 200
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

type rb_variedad from radiobutton within w_info_existencia_seguncamara
integer x = 411
integer y = 1452
integer width = 558
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
string text = "Por Variedad"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF this.checked THEN
	rb_controltodos.enabled = True
	rb_rechazados.enabled 	= True
	rb_objetados.enabled 	= True
	rb_habilitado.enabled 	= True
	gb_6.enabled 				= True
END IF
end event

type rb_embalaje from radiobutton within w_info_existencia_seguncamara
integer x = 1065
integer y = 1452
integer width = 558
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
string text = "Por Embalaje"
boolean lefttext = true
end type

event clicked;if this.checked then
	rb_controltodos.enabled = true
	rb_rechazados.enabled 	= true
	rb_objetados.enabled 	= true
	rb_habilitado.enabled 	= true
	gb_6.enabled 				= true
end if
end event

type st_productor from statictext within w_info_existencia_seguncamara
integer x = 1769
integer y = 812
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
string text = "Productor"
boolean focusrectangle = false
end type

type dw_stat from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 165
integer y = 1980
integer width = 969
integer height = 88
integer taborder = 250
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

type dw_1 from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 1472
integer y = 1896
integer width = 635
integer height = 160
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_existencia_frigovari"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 215
integer y = 2084
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificopredios"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 567
integer y = 2084
integer width = 686
integer height = 400
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_existencia_frigopalletporcamara"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 791
integer y = 2084
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_palletpucho"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 1115
integer y = 2084
integer width = 686
integer height = 400
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_pallet_inspec"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 1550
integer y = 2084
integer width = 686
integer height = 400
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_pallet_condi"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_controltodos from radiobutton within w_info_existencia_seguncamara
integer x = 558
integer y = 1684
integer width = 283
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_control = -1
	is_control_d = this.text
end if
end event

type rb_rechazados from radiobutton within w_info_existencia_seguncamara
integer x = 1102
integer y = 1684
integer width = 471
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
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_control=3
	is_control_d = this.Text
END IF
end event

type rb_objetados from radiobutton within w_info_existencia_seguncamara
integer x = 1733
integer y = 1684
integer width = 407
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
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_control=2
	is_control_d = this.Text
END IF
end event

type rb_habilitado from radiobutton within w_info_existencia_seguncamara
integer x = 2414
integer y = 1684
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
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_control=1
	is_control_d = this.Text
END IF
end event

type gb_6 from groupbox within w_info_existencia_seguncamara
integer x = 279
integer y = 1616
integer width = 2912
integer height = 172
integer taborder = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Control Calidad"
end type

type st_2 from statictext within w_info_existencia_seguncamara
integer x = 242
integer y = 1588
integer width = 2990
integer height = 236
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

type dw_categoria from datawindow within w_info_existencia_seguncamara
integer x = 2158
integer y = 976
integer width = 878
integer height = 96
integer taborder = 260
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

type st_5 from statictext within w_info_existencia_seguncamara
integer x = 1765
integer y = 984
integer width = 343
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
string text = "Categorias"
boolean focusrectangle = false
end type

type cbx_categoria from checkbox within w_info_existencia_seguncamara
integer x = 2162
integer y = 900
integer width = 571
integer height = 76
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
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

type uo_selespecie from uo_seleccion_especie within w_info_existencia_seguncamara
event destroy ( )
integer x = 731
integer y = 628
integer height = 180
integer taborder = 140
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

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_seguncamara
event destroy ( )
integer x = 731
integer y = 836
integer taborder = 170
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_seguncamara
integer x = 1774
integer y = 1452
integer width = 663
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

type st_9 from statictext within w_info_existencia_seguncamara
integer x = 242
integer y = 1824
integer width = 2990
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_archivo from checkbox within w_info_existencia_seguncamara
integer x = 1307
integer y = 1832
integer width = 745
integer height = 80
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
string text = "Genera Archivo Excel"
end type

type dw_excel from datawindow within w_info_existencia_seguncamara
boolean visible = false
integer x = 2345
integer y = 1916
integer width = 686
integer height = 400
integer taborder = 270
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cbx_calibre from checkbox within w_info_existencia_seguncamara
integer x = 2464
integer y = 1452
integer width = 658
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
string text = "Calibre Rotulado"
end type

type st_1 from statictext within w_info_existencia_seguncamara
integer x = 1714
integer y = 608
integer width = 1518
integer height = 780
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

type st_8 from statictext within w_info_existencia_seguncamara
integer x = 302
integer y = 1140
integer width = 357
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calificación"
boolean focusrectangle = false
end type

type ddlb_calificacion from dropdownlistbox within w_info_existencia_seguncamara
integer x = 727
integer y = 1140
integer width = 480
integer height = 400
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
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

event selectionchanged;ii_tipocamara	= -1
end event

type cbx_consolcalifi from checkbox within w_info_existencia_seguncamara
integer x = 1211
integer y = 1048
integer width = 475
integer height = 76
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type cbx_todcalifi from checkbox within w_info_existencia_seguncamara
integer x = 731
integer y = 1048
integer width = 407
integer height = 72
integer taborder = 160
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
	
	ii_calificacion = -1
	ddlb_calificacion.SelectItem(0)
	ddlb_calificacion.Enabled = False
ELSE
	ddlb_calificacion.Enabled = True
	ii_calificacion = 0
	
END IF
end event

type st_embalaje from statictext within w_info_existencia_seguncamara
integer x = 302
integer y = 1284
integer width = 352
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

type em_embalaje from singlelineedit within w_info_existencia_seguncamara
integer x = 722
integer y = 1280
integer width = 297
integer height = 84
integer taborder = 290
boolean bringtotop = true
integer textsize = -10
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
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
istr_mant.argumento[8]	=	ls_embalaje
	
	
end event

type cb_buscaembalaje from commandbutton within w_info_existencia_seguncamara
integer x = 1029
integer y = 1288
integer width = 96
integer height = 76
integer taborder = 320
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

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[8]	=	lstr_busq.argum[2]
	
END IF
end event

type cbx_embalaje from checkbox within w_info_existencia_seguncamara
integer x = 1138
integer y = 1284
integer width = 265
integer height = 80
integer taborder = 310
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
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type st_7 from statictext within w_info_existencia_seguncamara
integer x = 1769
integer y = 1276
integer width = 306
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
string text = "Cámara"
boolean focusrectangle = false
end type

type dw_camara from datawindow within w_info_existencia_seguncamara
integer x = 2158
integer y = 1260
integer width = 1006
integer height = 92
integer taborder = 60
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_camaras_tipo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;
ii_camara	= Integer(data)

end event

type cbx_camaras from checkbox within w_info_existencia_seguncamara
integer x = 2171
integer y = 1184
integer width = 270
integer height = 76
integer taborder = 210
boolean bringtotop = true
integer textsize = -10
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
	dw_camara.Enabled		=	False
	ii_camara	=	-1
ELSE
	dw_camara.Enabled		=	True
	dw_camara.SetFocus()
END IF

end event

type dw_tipocamara from datawindow within w_info_existencia_seguncamara
integer x = 2153
integer y = 1076
integer width = 777
integer height = 116
integer taborder = 190
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_tipocamara_1"
boolean border = false
boolean livescroll = true
end type

event itemchanged;ii_tipocamara = Integer(data)

dw_camara.GetChild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(SQLCA)
idwc_camara.Retrieve(Integer(istr_mant.argumento[2]),ii_tipocamara)
dw_camara.InsertRow(0)

end event

type st_10 from statictext within w_info_existencia_seguncamara
integer x = 1765
integer y = 1116
integer width = 389
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
string text = "Tipo Cámara"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_info_existencia_seguncamara
integer x = 2903
integer y = 1100
integer width = 270
integer height = 76
integer taborder = 220
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
	dw_tipocamara.Enabled		=	False
	ii_tipocamara	=	0
	
	dw_camara.GetChild("cama_codigo", idwc_camara)
	idwc_camara.SetTransObject(SQLCA)
	idwc_camara.Retrieve(Integer(istr_mant.argumento[2]),ii_tipocamara)
	dw_camara.InsertRow(0)
ELSE
	dw_tipocamara.Enabled		=	True
	dw_tipocamara.SetFocus()
END IF

end event

type st_11 from statictext within w_info_existencia_seguncamara
integer x = 1746
integer y = 516
integer width = 238
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_existencia_seguncamara
integer x = 1938
integer y = 504
integer width = 987
integer height = 96
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
	
	dw_camara.GetChild("cama_codigo", idwc_camara)
	idwc_camara.SetTransObject(SQLCA)
	idwc_camara.Retrieve(Integer(istr_mant.argumento[2]),ii_tipocamara)
	dw_camara.InsertRow(0)

ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	RETURN 1
END IF


end event

event itemerror;RETURN 1
end event

type cbx_planta from checkbox within w_info_existencia_seguncamara
integer x = 1947
integer y = 448
integer width = 302
integer height = 60
integer taborder = 30
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

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'-1'
	
	dw_camara.GetChild("cama_codigo", idwc_camara)
	idwc_camara.SetTransObject(SQLCA)
	idwc_camara.Retrieve(-1,ii_tipocamara)
	dw_camara.InsertRow(0)
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_seguncamara
integer x = 2162
integer y = 628
integer taborder = 200
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event dragdrop;call super::dragdrop;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

type cbx_clientes from checkbox within w_info_existencia_seguncamara
integer x = 530
integer y = 448
integer width = 302
integer height = 60
integer taborder = 10
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
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled			=	False
	istr_mant.argumento[1]	=	'-1'
	
ELSE
	istr_mant.argumento[1]	=  String(dw_cliente.Object.clie_codigo[1])
	dw_cliente.Enabled			=	True
	
	dw_cliente.SetFocus()
END IF
end event

