$PBExportHeader$w_info_existencia_frigorificos.srw
forward
global type w_info_existencia_frigorificos from w_para_informes
end type
type dw_cliente from datawindow within w_info_existencia_frigorificos
end type
type st_6 from statictext within w_info_existencia_frigorificos
end type
type st_4 from statictext within w_info_existencia_frigorificos
end type
type st_especie from statictext within w_info_existencia_frigorificos
end type
type st_nro2 from statictext within w_info_existencia_frigorificos
end type
type st_variedad from statictext within w_info_existencia_frigorificos
end type
type st_nro3 from statictext within w_info_existencia_frigorificos
end type
type rb_variedad from radiobutton within w_info_existencia_frigorificos
end type
type rb_embalaje from radiobutton within w_info_existencia_frigorificos
end type
type rb_pallet from radiobutton within w_info_existencia_frigorificos
end type
type st_productor from statictext within w_info_existencia_frigorificos
end type
type rb_palletcal from radiobutton within w_info_existencia_frigorificos
end type
type rb_1 from radiobutton within w_info_existencia_frigorificos
end type
type rb_inspeccion from radiobutton within w_info_existencia_frigorificos
end type
type cbx_status from checkbox within w_info_existencia_frigorificos
end type
type dw_stat from datawindow within w_info_existencia_frigorificos
end type
type dw_1 from datawindow within w_info_existencia_frigorificos
end type
type dw_2 from datawindow within w_info_existencia_frigorificos
end type
type dw_3 from datawindow within w_info_existencia_frigorificos
end type
type dw_4 from datawindow within w_info_existencia_frigorificos
end type
type dw_5 from datawindow within w_info_existencia_frigorificos
end type
type dw_6 from datawindow within w_info_existencia_frigorificos
end type
type rb_controltodos from radiobutton within w_info_existencia_frigorificos
end type
type rb_rechazados from radiobutton within w_info_existencia_frigorificos
end type
type rb_objetados from radiobutton within w_info_existencia_frigorificos
end type
type rb_habilitado from radiobutton within w_info_existencia_frigorificos
end type
type gb_6 from groupbox within w_info_existencia_frigorificos
end type
type st_2 from statictext within w_info_existencia_frigorificos
end type
type rb_predio from checkbox within w_info_existencia_frigorificos
end type
type st_3 from statictext within w_info_existencia_frigorificos
end type
type dw_categoria from datawindow within w_info_existencia_frigorificos
end type
type st_5 from statictext within w_info_existencia_frigorificos
end type
type cbx_categoria from checkbox within w_info_existencia_frigorificos
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_frigorificos
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_frigorificos
end type
type cbx_varirotula from checkbox within w_info_existencia_frigorificos
end type
type cbx_tipofrio from checkbox within w_info_existencia_frigorificos
end type
type st_7 from statictext within w_info_existencia_frigorificos
end type
type st_9 from statictext within w_info_existencia_frigorificos
end type
type cbx_archivo from checkbox within w_info_existencia_frigorificos
end type
type dw_excel from datawindow within w_info_existencia_frigorificos
end type
type cbx_packing from checkbox within w_info_existencia_frigorificos
end type
type cbx_calibre from checkbox within w_info_existencia_frigorificos
end type
type st_8 from statictext within w_info_existencia_frigorificos
end type
type ddlb_calificacion from dropdownlistbox within w_info_existencia_frigorificos
end type
type cbx_consolcalifi from checkbox within w_info_existencia_frigorificos
end type
type cbx_todcalifi from checkbox within w_info_existencia_frigorificos
end type
type st_embalaje from statictext within w_info_existencia_frigorificos
end type
type em_embalaje from singlelineedit within w_info_existencia_frigorificos
end type
type cb_buscaembalaje from commandbutton within w_info_existencia_frigorificos
end type
type cbx_embalaje from checkbox within w_info_existencia_frigorificos
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_frigorificos
end type
type st_1 from statictext within w_info_existencia_frigorificos
end type
type st_10 from statictext within w_info_existencia_frigorificos
end type
type cbx_todcalifi2 from checkbox within w_info_existencia_frigorificos
end type
type cbx_consolcalifi2 from checkbox within w_info_existencia_frigorificos
end type
type ddlb_calificacion2 from dropdownlistbox within w_info_existencia_frigorificos
end type
end forward

global type w_info_existencia_frigorificos from w_para_informes
integer x = 14
integer y = 32
integer width = 3799
integer height = 2312
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
rb_pallet rb_pallet
st_productor st_productor
rb_palletcal rb_palletcal
rb_1 rb_1
rb_inspeccion rb_inspeccion
cbx_status cbx_status
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
rb_predio rb_predio
st_3 st_3
dw_categoria dw_categoria
st_5 st_5
cbx_categoria cbx_categoria
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_tipofrio cbx_tipofrio
st_7 st_7
st_9 st_9
cbx_archivo cbx_archivo
dw_excel dw_excel
cbx_packing cbx_packing
cbx_calibre cbx_calibre
st_8 st_8
ddlb_calificacion ddlb_calificacion
cbx_consolcalifi cbx_consolcalifi
cbx_todcalifi cbx_todcalifi
st_embalaje st_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_embalaje cbx_embalaje
uo_selproductor uo_selproductor
st_1 st_1
st_10 st_10
cbx_todcalifi2 cbx_todcalifi2
cbx_consolcalifi2 cbx_consolcalifi2
ddlb_calificacion2 ddlb_calificacion2
end type
global w_info_existencia_frigorificos w_info_existencia_frigorificos

type variables
str_busqueda 	istr_busq
str_mant 		istr_mant

String is_control_d
Integer ii_control, ii_calificacion, ii_calificacion2

DataWindowChild	idwc_cliente, idwc_especie, idwc_stat, idwc_categorias

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

on w_info_existencia_frigorificos.create
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
this.rb_pallet=create rb_pallet
this.st_productor=create st_productor
this.rb_palletcal=create rb_palletcal
this.rb_1=create rb_1
this.rb_inspeccion=create rb_inspeccion
this.cbx_status=create cbx_status
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
this.rb_predio=create rb_predio
this.st_3=create st_3
this.dw_categoria=create dw_categoria
this.st_5=create st_5
this.cbx_categoria=create cbx_categoria
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_tipofrio=create cbx_tipofrio
this.st_7=create st_7
this.st_9=create st_9
this.cbx_archivo=create cbx_archivo
this.dw_excel=create dw_excel
this.cbx_packing=create cbx_packing
this.cbx_calibre=create cbx_calibre
this.st_8=create st_8
this.ddlb_calificacion=create ddlb_calificacion
this.cbx_consolcalifi=create cbx_consolcalifi
this.cbx_todcalifi=create cbx_todcalifi
this.st_embalaje=create st_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_embalaje=create cbx_embalaje
this.uo_selproductor=create uo_selproductor
this.st_1=create st_1
this.st_10=create st_10
this.cbx_todcalifi2=create cbx_todcalifi2
this.cbx_consolcalifi2=create cbx_consolcalifi2
this.ddlb_calificacion2=create ddlb_calificacion2
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
this.Control[iCurrent+10]=this.rb_pallet
this.Control[iCurrent+11]=this.st_productor
this.Control[iCurrent+12]=this.rb_palletcal
this.Control[iCurrent+13]=this.rb_1
this.Control[iCurrent+14]=this.rb_inspeccion
this.Control[iCurrent+15]=this.cbx_status
this.Control[iCurrent+16]=this.dw_stat
this.Control[iCurrent+17]=this.dw_1
this.Control[iCurrent+18]=this.dw_2
this.Control[iCurrent+19]=this.dw_3
this.Control[iCurrent+20]=this.dw_4
this.Control[iCurrent+21]=this.dw_5
this.Control[iCurrent+22]=this.dw_6
this.Control[iCurrent+23]=this.rb_controltodos
this.Control[iCurrent+24]=this.rb_rechazados
this.Control[iCurrent+25]=this.rb_objetados
this.Control[iCurrent+26]=this.rb_habilitado
this.Control[iCurrent+27]=this.gb_6
this.Control[iCurrent+28]=this.st_2
this.Control[iCurrent+29]=this.rb_predio
this.Control[iCurrent+30]=this.st_3
this.Control[iCurrent+31]=this.dw_categoria
this.Control[iCurrent+32]=this.st_5
this.Control[iCurrent+33]=this.cbx_categoria
this.Control[iCurrent+34]=this.uo_selespecie
this.Control[iCurrent+35]=this.uo_selvariedad
this.Control[iCurrent+36]=this.cbx_varirotula
this.Control[iCurrent+37]=this.cbx_tipofrio
this.Control[iCurrent+38]=this.st_7
this.Control[iCurrent+39]=this.st_9
this.Control[iCurrent+40]=this.cbx_archivo
this.Control[iCurrent+41]=this.dw_excel
this.Control[iCurrent+42]=this.cbx_packing
this.Control[iCurrent+43]=this.cbx_calibre
this.Control[iCurrent+44]=this.st_8
this.Control[iCurrent+45]=this.ddlb_calificacion
this.Control[iCurrent+46]=this.cbx_consolcalifi
this.Control[iCurrent+47]=this.cbx_todcalifi
this.Control[iCurrent+48]=this.st_embalaje
this.Control[iCurrent+49]=this.em_embalaje
this.Control[iCurrent+50]=this.cb_buscaembalaje
this.Control[iCurrent+51]=this.cbx_embalaje
this.Control[iCurrent+52]=this.uo_selproductor
this.Control[iCurrent+53]=this.st_1
this.Control[iCurrent+54]=this.st_10
this.Control[iCurrent+55]=this.cbx_todcalifi2
this.Control[iCurrent+56]=this.cbx_consolcalifi2
this.Control[iCurrent+57]=this.ddlb_calificacion2
end on

on w_info_existencia_frigorificos.destroy
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
destroy(this.rb_pallet)
destroy(this.st_productor)
destroy(this.rb_palletcal)
destroy(this.rb_1)
destroy(this.rb_inspeccion)
destroy(this.cbx_status)
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
destroy(this.rb_predio)
destroy(this.st_3)
destroy(this.dw_categoria)
destroy(this.st_5)
destroy(this.cbx_categoria)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_tipofrio)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.cbx_archivo)
destroy(this.dw_excel)
destroy(this.cbx_packing)
destroy(this.cbx_calibre)
destroy(this.st_8)
destroy(this.ddlb_calificacion)
destroy(this.cbx_consolcalifi)
destroy(this.cbx_todcalifi)
destroy(this.st_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_embalaje)
destroy(this.uo_selproductor)
destroy(this.st_1)
destroy(this.st_10)
destroy(this.cbx_todcalifi2)
destroy(this.cbx_consolcalifi2)
destroy(this.ddlb_calificacion2)
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
ii_calificacion2 = -9

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
end event

event resize;call super::resize;//pb_acepta.x			=	This.WorkSpaceWidth() - 292
//pb_acepta.y			=	gb_1.y + 88
//pb_acepta.width	=	156
//pb_acepta.height	=	133
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_frigorificos
integer x = 3387
integer y = 756
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_frigorificos
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_frigorificos
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_frigorificos
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_frigorificos
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_frigorificos
integer width = 2990
string text = "Existencia Almacenada en Frigorificos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_frigorificos
string tag = "Imprimir Reporte"
integer x = 3406
integer y = 1520
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_Cliente, li_categoria, li_varirotula, li_tipofrio,li_packing, li_calibre,&
			li_emba, li_emba2
String	ls_descri, ls_cliente, ls_categoria, ls_Archivo, ls_ruta, ls_lista, ls_embalaje,&
			ls_string, ls_construye, ls_construyelike1, ls_construyelike
Long		ll_produ
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
	li_packing = 1
ELSE
	li_packing = 0
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

IF cbx_archivo.Checked THEN
	IF rb_variedad.checked  THEN
		IF rb_predio.Checked  THEN
			dw_excel.DataObject = "dw_info_excel_frigorificopredios"
			lb_flag_retrieve		= true
			//1
		ELSE	
			dw_excel.DataObject = "dw_info_excel_frigorificos"
			lb_flag_retrieve		= true
			//1
		END IF	
	END IF
	
	IF rb_embalaje.checked THEN
		dw_excel.DataObject = "dw_info_excel_frigorificos"
		lb_flag_retrieve		= true
		//4
	ELSEIF rb_pallet.checked THEN
		dw_excel.DataObject = "dw_info_excel_frigorificos_palletpucho"
		lb_flag_retrieve		= false
		//2
	ELSEIF rb_palletcal.Checked THEN
		dw_excel.DataObject = "dw_info_excel_frigorificos_palletpucho"
		lb_flag_retrieve		= false
		//5
	ELSEIF rb_inspeccion.Checked THEN
		dw_excel.DataObject = "dw_info_excel_frigorificos_pallet_inspec"
		lb_flag_retrieve		= true
		//6
	ELSEIF rb_1.Checked	THEN
		dw_excel.DataObject = "dw_info_excel_frigorificos_pallet_condi"
		lb_flag_retrieve		= true
		//3
	END IF
	
ELSE
	IF rb_variedad.checked  THEN
		IF rb_predio.Checked  THEN
			vinf.dw_1.DataObject = "dw_existencia_frigovariedad_predio"
			lb_flag_retrieve		= true
			//1
		ELSE	
			vinf.dw_1.DataObject = "dw_existencia_frigovariedad"
			lb_flag_retrieve		= true
			//1
		END IF	
	END IF
	
	IF rb_embalaje.checked THEN
		vinf.dw_1.DataObject = "dw_existencia_frigoembalaje"
		lb_flag_retrieve		= true
		//4
	ELSEIF rb_pallet.checked THEN
		vinf.dw_1.DataObject = "dw_existencia_frigopalletpucho"
		lb_flag_retrieve		= false
		//2
	ELSEIF rb_palletcal.Checked THEN
		vinf.dw_1.DataObject = "dw_existencia_frigopalletpucho_ecal"
		lb_flag_retrieve		= false
		//5
	ELSEIF rb_inspeccion.Checked THEN
		vinf.dw_1.DataObject = "dw_existencia_frigopalletinspeccion"
		lb_flag_retrieve		= true
		//6
	ELSEIF rb_1.Checked	THEN
		vinf.dw_1.DataObject = "dw_existencia_frigopalletcondicion"
		lb_flag_retrieve		= true
		//3
	END IF
END IF
vinf.dw_1.SetTransObject(sqlca)

dw_excel.SetTransObject(sqlca)

IF cbx_archivo.Checked THEN
	IF lb_flag_retrieve then
		fila	=	dw_excel.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
											 uo_selvariedad.Codigo,&
											 integer(istr_mant.argumento[6]), ii_control, &
											 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
											 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,string(ii_calificacion2))
	ELSE
		fila	=	dw_excel.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
											 uo_selvariedad.Codigo, &
											 integer(istr_mant.argumento[6]),integer(istr_mant.argumento[10]),&
											 li_varirotula,li_tipofrio,li_calibre,ls_lista,ii_calificacion,ls_construyelike,string(ii_calificacion2))
	END IF
	
	ls_Archivo	=	"\ExistenciaFrigorifico.xls"
		
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
			
	dw_excel.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)
	MessageBox("Atención","Archivo Formato Excel, Generado.")
	
ELSE
	IF lb_flag_retrieve then
		fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
											 uo_selvariedad.Codigo, &
											 integer(istr_mant.argumento[6]), ii_control, &
											 integer(istr_mant.argumento[10]), li_varirotula,li_tipofrio,&
											 li_packing,li_calibre,ls_lista,ii_calificacion,ls_construyelike,string(ii_calificacion2))
	ELSE
		fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),uo_selespecie.Codigo, &
											 uo_selvariedad.Codigo, &
											 integer(istr_mant.argumento[6]),integer(istr_mant.argumento[10]),&
											 li_varirotula,li_tipofrio,li_calibre,ls_lista,ii_calificacion,ls_construyelike,string(ii_calificacion2))
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
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_frigorificos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3415
integer y = 1884
integer taborder = 100
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_cliente from datawindow within w_info_existencia_frigorificos
integer x = 1189
integer y = 424
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

type st_6 from statictext within w_info_existencia_frigorificos
integer x = 850
integer y = 432
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

type st_4 from statictext within w_info_existencia_frigorificos
integer x = 247
integer y = 408
integer width = 2990
integer height = 120
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_especie from statictext within w_info_existencia_frigorificos
integer x = 297
integer y = 624
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

type st_nro2 from statictext within w_info_existencia_frigorificos
integer x = 247
integer y = 528
integer width = 1472
integer height = 728
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_existencia_frigorificos
integer x = 297
integer y = 796
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

type st_nro3 from statictext within w_info_existencia_frigorificos
integer x = 247
integer y = 1388
integer width = 2990
integer height = 388
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_variedad from radiobutton within w_info_existencia_frigorificos
integer x = 411
integer y = 1440
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
	rb_predio.Enabled			= True
ELSE
	rb_predio.Enabled			= False
	rb_predio.Checked			= False
END IF
end event

type rb_embalaje from radiobutton within w_info_existencia_frigorificos
integer x = 411
integer y = 1512
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
	rb_predio.Enabled			= False
	rb_predio.Checked			= False
end if
end event

type rb_pallet from radiobutton within w_info_existencia_frigorificos
integer x = 1883
integer y = 1440
integer width = 1125
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
string text = "Pallets/Puchos/Variedad Embalaje"
end type

event clicked;if this.checked then
	rb_controltodos.enabled = false
	rb_rechazados.enabled 	= false
	rb_objetados.enabled 	= false
	rb_habilitado.enabled 	= false
	gb_6.enabled 				= false
	rb_predio.Enabled			= False
	rb_predio.Checked			= False
end if
end event

type st_productor from statictext within w_info_existencia_frigorificos
integer x = 1769
integer y = 692
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

type rb_palletcal from radiobutton within w_info_existencia_frigorificos
integer x = 1883
integer y = 1588
integer width = 1138
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
string text = "Pallets/Puchos/Variedad Calibre"
end type

event clicked;if this.checked then
	rb_controltodos.enabled = false
	rb_rechazados.enabled 	= false
	rb_objetados.enabled 	= false
	rb_habilitado.enabled 	= false
	gb_6.enabled 				= false
	rb_predio.Enabled			= False
	rb_predio.Checked			= False
end if
end event

type rb_1 from radiobutton within w_info_existencia_frigorificos
integer x = 1883
integer y = 1512
integer width = 1125
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
string text = "Pallets/Puchos/Variedad Condición"
end type

event clicked;if this.checked then
	rb_controltodos.enabled = true
	rb_rechazados.enabled 	= true
	rb_objetados.enabled 	= true
	rb_habilitado.enabled 	= true
	gb_6.enabled 				= true
	rb_predio.Enabled			= False
	rb_predio.Checked			= False
end if
end event

type rb_inspeccion from radiobutton within w_info_existencia_frigorificos
integer x = 1883
integer y = 1660
integer width = 1138
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
string text = "Pallets/Puchos/Variedad Inspección"
end type

event clicked;if this.checked then
	rb_controltodos.enabled = true
	rb_rechazados.enabled 	= true
	rb_objetados.enabled 	= true
	rb_habilitado.enabled 	= true
	gb_6.enabled 				= true
	rb_predio.Enabled			= False
	rb_predio.Checked			= False
end if
end event

type cbx_status from checkbox within w_info_existencia_frigorificos
integer x = 2469
integer y = 1284
integer width = 741
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

type dw_stat from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 1888
integer y = 2328
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

type dw_1 from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 3237
integer y = 2276
integer width = 169
integer height = 144
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_existencia_frigovari"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 3415
integer y = 2272
integer width = 192
integer height = 152
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificopredios"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 3616
integer y = 2272
integer width = 197
integer height = 152
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 3813
integer y = 2276
integer width = 169
integer height = 136
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_palletpucho"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 3246
integer y = 2432
integer width = 169
integer height = 144
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_pallet_inspec"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 3415
integer y = 2436
integer width = 206
integer height = 132
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_excel_frigorificos_pallet_condi"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_controltodos from radiobutton within w_info_existencia_frigorificos
integer x = 562
integer y = 1860
integer width = 283
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_control = -1
	is_control_d = this.text
end if
end event

type rb_rechazados from radiobutton within w_info_existencia_frigorificos
integer x = 1106
integer y = 1860
integer width = 471
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
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_control=3
	is_control_d = this.Text
END IF
end event

type rb_objetados from radiobutton within w_info_existencia_frigorificos
integer x = 1737
integer y = 1860
integer width = 407
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
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_control=2
	is_control_d = this.Text
END IF
end event

type rb_habilitado from radiobutton within w_info_existencia_frigorificos
integer x = 2423
integer y = 1860
integer width = 434
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
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_control=1
	is_control_d = this.Text
END IF
end event

type gb_6 from groupbox within w_info_existencia_frigorificos
integer x = 283
integer y = 1792
integer width = 2912
integer height = 152
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Control Calidad"
end type

type st_2 from statictext within w_info_existencia_frigorificos
integer x = 247
integer y = 1776
integer width = 2990
integer height = 192
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_predio from checkbox within w_info_existencia_frigorificos
integer x = 402
integer y = 1660
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
string text = "Por Predio"
end type

type st_3 from statictext within w_info_existencia_frigorificos
integer x = 247
integer y = 1252
integer width = 1472
integer height = 136
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_categoria from datawindow within w_info_existencia_frigorificos
integer x = 2158
integer y = 1076
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

type st_5 from statictext within w_info_existencia_frigorificos
integer x = 1769
integer y = 1084
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

type cbx_categoria from checkbox within w_info_existencia_frigorificos
integer x = 2167
integer y = 1000
integer width = 571
integer height = 76
integer taborder = 60
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

type uo_selespecie from uo_seleccion_especie within w_info_existencia_frigorificos
event destroy ( )
integer x = 736
integer y = 536
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

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_frigorificos
event destroy ( )
integer x = 736
integer y = 708
integer taborder = 70
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_frigorificos
integer x = 288
integer y = 1284
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

type cbx_tipofrio from checkbox within w_info_existencia_frigorificos
integer x = 1778
integer y = 1284
integer width = 585
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
string text = "Según Tipo Frío  "
end type

type st_7 from statictext within w_info_existencia_frigorificos
integer x = 1719
integer y = 1256
integer width = 1518
integer height = 132
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_existencia_frigorificos
integer x = 247
integer y = 1968
integer width = 2990
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_archivo from checkbox within w_info_existencia_frigorificos
integer x = 1312
integer y = 1976
integer width = 745
integer height = 80
integer taborder = 50
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

type dw_excel from datawindow within w_info_existencia_frigorificos
boolean visible = false
integer x = 2935
integer y = 2304
integer width = 261
integer height = 188
integer taborder = 100
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cbx_packing from checkbox within w_info_existencia_frigorificos
integer x = 402
integer y = 1584
integer width = 791
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
string text = "Incluye Existencia Packing"
end type

event clicked;IF This.Checked THEN
	rb_pallet.Enabled 		= False
	rb_1.Enabled 				= False
	rb_palletcal.Enabled 	= False
	rb_inspeccion.Enabled 	= False
	
	rb_pallet.Checked 		= False
	rb_1.Checked 				= False
	rb_palletcal.Checked 	= False
	rb_inspeccion.Checked 	= False
	
	rb_variedad.Checked		= True
	rb_predio.Enabled			= True
ELSE
	rb_pallet.Enabled 		= True
	rb_1.Enabled 				= True
	rb_palletcal.Enabled 	= True
	rb_inspeccion.Enabled 	= True
	
END IF	
end event

type cbx_calibre from checkbox within w_info_existencia_frigorificos
integer x = 983
integer y = 1284
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

type st_8 from statictext within w_info_existencia_frigorificos
integer x = 297
integer y = 952
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

type ddlb_calificacion from dropdownlistbox within w_info_existencia_frigorificos
integer x = 731
integer y = 948
integer width = 480
integer height = 400
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
boolean sorted = false
string item[] = {"Sin Calif.","1","2","3","4","5"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF (index) = 1 THEN
	ii_calificacion	=	0//(index)
ELSE
	ii_calificacion	=	(index) -1
END IF	

end event

type cbx_consolcalifi from checkbox within w_info_existencia_frigorificos
integer x = 1216
integer y = 884
integer width = 475
integer height = 76
integer taborder = 60
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

type cbx_todcalifi from checkbox within w_info_existencia_frigorificos
integer x = 736
integer y = 884
integer width = 407
integer height = 72
integer taborder = 60
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

type st_embalaje from statictext within w_info_existencia_frigorificos
integer x = 1769
integer y = 880
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

type em_embalaje from singlelineedit within w_info_existencia_frigorificos
integer x = 2162
integer y = 876
integer width = 297
integer height = 84
integer taborder = 120
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

type cb_buscaembalaje from commandbutton within w_info_existencia_frigorificos
integer x = 2469
integer y = 884
integer width = 96
integer height = 76
integer taborder = 210
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

type cbx_embalaje from checkbox within w_info_existencia_frigorificos
integer x = 2578
integer y = 880
integer width = 265
integer height = 80
integer taborder = 170
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

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_existencia_frigorificos
integer x = 2162
integer y = 548
integer taborder = 80
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

type st_1 from statictext within w_info_existencia_frigorificos
integer x = 1719
integer y = 528
integer width = 1518
integer height = 728
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_existencia_frigorificos
integer x = 297
integer y = 1120
integer width = 407
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
string text = "Calificación 2"
boolean focusrectangle = false
end type

type cbx_todcalifi2 from checkbox within w_info_existencia_frigorificos
integer x = 736
integer y = 1068
integer width = 407
integer height = 72
integer taborder = 60
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
	
	ii_calificacion2 = -1
	ddlb_calificacion2.SelectItem(0)
	ddlb_calificacion2.Enabled = False
ELSE
	ddlb_calificacion2.Enabled = True
	ii_calificacion2 = 0
	
END IF
end event

type cbx_consolcalifi2 from checkbox within w_info_existencia_frigorificos
integer x = 1216
integer y = 1068
integer width = 475
integer height = 76
integer taborder = 60
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
	
	cbx_todcalifi2.Enabled = False
	ddlb_calificacion2.SelectItem(0)
	cbx_todcalifi2.Checked = True
	ddlb_calificacion2.Enabled = False
	ii_calificacion2 = -9
	
ELSE	
	cbx_todcalifi2.Enabled = True
	ii_calificacion2 = -1
END IF
end event

type ddlb_calificacion2 from dropdownlistbox within w_info_existencia_frigorificos
integer x = 731
integer y = 1136
integer width = 480
integer height = 400
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
boolean sorted = false
string item[] = {"Sin Calif.","1","2","3","4","5"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF (index) = 1 THEN
	ii_calificacion2	=	0//(index)
ELSE
	ii_calificacion2	=	(index) -1
END IF	

end event

